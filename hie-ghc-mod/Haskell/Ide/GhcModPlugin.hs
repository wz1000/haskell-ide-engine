{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Haskell.Ide.GhcModPlugin where

import           Bag
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Function
import           Data.IORef
import           Data.List
import qualified Data.Map                          as Map
import           Data.Monoid
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import           DynFlags
import           ErrUtils
import qualified Exception                         as G
import           GHC
import           GHC.Generics
import qualified GhcMod                            as GM
import qualified GhcMod.Doc                        as GM
import qualified GhcMod.DynFlags                   as GM
import qualified GhcMod.Error                      as GM
import qualified GhcMod.Gap                        as GM
import qualified GhcMod.Monad                      as GM
import qualified GhcMod.SrcUtils                   as GM
import qualified GhcMod.Types                      as GM
import qualified GhcMod.Utils                      as GM
import           Haskell.Ide.Engine.IdeFunctions
import           Haskell.Ide.Engine.LocMap
import           Haskell.Ide.Engine.ModuleLoader
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           HscTypes
import           Outputable                        (renderWithStyle)

-- ---------------------------------------------------------------------

ghcmodDescriptor :: PluginDescriptor
ghcmodDescriptor = PluginDescriptor
  {
    pluginName = "ghc-mod"
  , pluginDesc = "ghc-mod is a backend program to enrich Haskell programming "
              <> "in editors. It strives to offer most of the features one has come to expect "
              <> "from modern IDEs in any editor."
  , pluginCommands =
      [ PluginCommand "check" "check a file for GHC warnings and errors" checkCmd
      , PluginCommand "lint" "Check files using `hlint'" lintCmd
      , PluginCommand "info" "Look up an identifier in the context of FILE (like ghci's `:info')" infoCmd
      , PluginCommand "type" "Get the type of the expression under (LINE,COL)" typeCmd
      ]
  }

-- ---------------------------------------------------------------------

type Diagnostics = Map.Map Uri (Set.Set Diagnostic)
type AdditionalErrs = [T.Text]

checkCmd :: CommandFunc Uri (Diagnostics, AdditionalErrs)
checkCmd = CmdSync $ \uri ->
  setTypecheckedModule uri

-- ---------------------------------------------------------------------

lspSev :: Severity -> DiagnosticSeverity
lspSev SevWarning = DsWarning
lspSev SevError   = DsError
lspSev SevFatal   = DsError
lspSev SevInfo    = DsInfo
lspSev _          = DsInfo

logDiag :: (FilePath -> FilePath) -> IORef AdditionalErrs -> IORef Diagnostics -> LogAction
logDiag rfm eref dref df _reason sev spn style msg = do
  eloc <- srcSpan2Loc rfm spn
  let msgTxt = T.pack $ renderWithStyle df msg style
  case eloc of
    Right (Location uri range) -> do
      let update = Map.insertWith' Set.union uri l
            where l = Set.singleton diag
          diag = Diagnostic range (Just $ lspSev sev) Nothing (Just "ghcmod") msgTxt
      modifyIORef' dref update
    Left _ -> do
      modifyIORef' eref (msgTxt:)
      return ()

unhelpfulSrcSpanErr :: T.Text -> IdeFailure
unhelpfulSrcSpanErr err =
  IdeRFail $
    IdeError PluginError
             ("Unhelpful SrcSpan" <> ": \"" <> err <> "\"")
             Null

srcErrToDiag :: MonadIO m
  => DynFlags
  -> (FilePath -> FilePath)
  -> SourceError -> m (Diagnostics, AdditionalErrs)
srcErrToDiag df rfm se = do
  debugm "in srcErrToDiag"
  let errMsgs = bagToList $ srcErrorMessages se
      processMsg err = do
        let sev = Just DsError
            unqual = errMsgContext err
            st = GM.mkErrStyle' df unqual
            msgTxt = T.pack $ renderWithStyle df (pprLocErrMsg err) st
        eloc <- srcSpan2Loc rfm $ errMsgSpan err
        case eloc of
          Right (Location uri range) ->
            return $ Right (uri, Diagnostic range sev Nothing (Just "ghcmod") msgTxt)
          Left _ -> return $ Left msgTxt
      processMsgs [] = return (Map.empty,[])
      processMsgs (x:xs) = do
        res <- processMsg x
        (m,es) <- processMsgs xs
        case res of
          Right (uri, diag) ->
            return (Map.insertWith' Set.union uri (Set.singleton diag) m, es)
          Left e -> return (m, e:es)
  processMsgs errMsgs

myLogger :: GM.IOish m
  => (FilePath -> FilePath)
  -> GM.GmlT m ()
  -> GM.GmlT m (Diagnostics, AdditionalErrs)
myLogger rfm action = do
  env <- getSession
  diagRef <- liftIO $ newIORef Map.empty
  errRef <- liftIO $ newIORef []
  let setLogger df = df { log_action = logDiag rfm errRef diagRef }
      ghcErrRes msg = (Map.empty, [T.pack msg])
      handlers =
        [ GM.GHandler $ \ex ->
            srcErrToDiag (hsc_dflags env) rfm ex
        , GM.GHandler $ \ex ->
            return $ ghcErrRes $ GM.renderGm $ GM.ghcExceptionDoc ex
        , GM.GHandler $ \(ex :: GM.SomeException) ->
            return (Map.empty ,[T.pack (show ex)])
        ]
      action' = do
        GM.withDynFlags setLogger action
        diags <- liftIO $ readIORef diagRef
        errs <- liftIO $ readIORef errRef
        return (diags,errs)
  GM.gcatches action' handlers

setTypecheckedModule :: Uri -> IdeM (IdeResponse (Diagnostics, AdditionalErrs))
setTypecheckedModule uri =
  pluginGetFile "setTypecheckedModule: " uri $ \fp -> do
    rfm <- GM.mkRevRedirMapFunc
    ((diags', errs), mtm) <- getTypecheckedModuleGhc (myLogger rfm) fp
    let diags = Map.insertWith' Set.union uri Set.empty diags'
    case mtm of
      Nothing -> do
        debugm $ "setTypecheckedModule: Didn't get typechecked module for: " ++ show fp
        return $ IdeResponseOk (diags,errs)
      Just tm -> do
        let cm = CachedModule tm (genLocMap tm) rfm return return
        cacheModule uri cm
        return $ IdeResponseOk (diags,errs)

-- ---------------------------------------------------------------------

lintCmd :: CommandFunc Uri T.Text
lintCmd = CmdSync $ \uri ->
  lintCmd' uri

lintCmd' :: Uri -> IdeM (IdeResponse T.Text)
lintCmd' uri =
  pluginGetFile "lint: " uri $ \file ->
    fmap T.pack <$> runGhcModCommand (GM.lint GM.defaultLintOpts file)

-- ---------------------------------------------------------------------

customOptions :: Options
customOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2}

data InfoParams =
  IP { ipFile :: Uri
     , ipExpr :: T.Text
     } deriving (Eq,Show,Generic)

instance FromJSON InfoParams where
  parseJSON = genericParseJSON customOptions
instance ToJSON InfoParams where
  toJSON = genericToJSON customOptions

infoCmd :: CommandFunc InfoParams T.Text
infoCmd = CmdSync $ \(IP uri expr) ->
  infoCmd' uri expr

infoCmd' :: Uri -> T.Text -> IdeM (IdeResponse T.Text)
infoCmd' uri expr =
  pluginGetFile "info: " uri $ \file ->
    fmap T.pack <$> runGhcModCommand (GM.info file (GM.Expression (T.unpack expr)))

-- ---------------------------------------------------------------------

data TypeParams =
  TP { tpIncludeConstraints :: Bool
     , tpFile               :: Uri
     , tpPos                :: Position
     } deriving (Eq,Show,Generic)

instance FromJSON TypeParams where
  parseJSON = genericParseJSON customOptions
instance ToJSON TypeParams where
  toJSON = genericToJSON customOptions

typeCmd :: CommandFunc TypeParams [(Range,T.Text)]
typeCmd = CmdSync $ \(TP bool uri pos) -> do
  _ <- setTypecheckedModule uri
  newTypeCmd bool uri pos

someErr :: String -> String -> IdeResponse a
someErr meth err =
  IdeResponseFail $
    IdeError PluginError
             (T.pack $ meth <> ": " <> err)
             Null

newTypeCmd :: Bool -> Uri -> Position -> IdeM (IdeResponse [(Range, T.Text)])
newTypeCmd bool uri newPos =
  let handlers  = [GM.GHandler $ \(ex :: GM.SomeException) ->
                     return $ someErr "newTypeCmd" (show ex)
                  ] in
  flip GM.gcatches handlers $ do
    mcm <- getCachedModule uri
    case mcm of
      Nothing -> return $ IdeResponseOk []
      Just cm -> do
        let mOldPos = newPosToOld cm newPos
        case mOldPos of
          Nothing -> return $ IdeResponseOk []
          Just pos ->
            GM.unGmlT $ GM.withInteractiveContext $ do
              let tm = tcMod cm
              spanTypes' <- GM.collectSpansTypes bool tm $ unPos pos
              let spanTypes = sortBy (GM.cmp `on` fst) spanTypes'
              dflag        <- getSessionDynFlags
              st           <- GM.getStyle
              let f (spn, t) = do
                    let range' = srcSpan2Range spn
                    case oldRangeToNew cm <$> range' of
                      (Right (Just range)) -> [(range , T.pack $ GM.pretty dflag st t)]
                      _ -> []
              return $ IdeResponseOk $ concatMap f spanTypes

getDynFlags :: IdeM DynFlags
getDynFlags = GM.unGmlT getSessionDynFlags

-- ---------------------------------------------------------------------

runGhcModCommand :: IdeM a
                 -> IdeM (IdeResponse a)
runGhcModCommand cmd =
  (IdeResponseOk <$> cmd) `G.gcatch`
    \(e :: GM.GhcModError) ->
      return $
      IdeResponseFail $
      IdeError PluginError (T.pack $ "hie-ghc-mod: " ++ show e) Null
