{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Experimenting with a data structure to define a plugin.
--
-- The general idea is that a given plugin returns this structure during the
-- initial load/registration process, when- or however this eventually happens.
--
-- It should define the following things
--  1. What features the plugin should expose into the IDE
--       (this one may not be needed initially)
--
--       This may include a requirement to store private data of a particular
--       form.
--
--       It may be interesting to look at the Android model wrt Intents and
--       shared resource management, e.g. default Calendar app, default SMS app,
--       all making use of Contacts service.

module Haskell.Ide.Engine.PluginDescriptor
  (
  -- * Plugins
    PluginId
  , CommandName
  , CommandFunc(..)
  , PluginDescriptor(..)
  , PluginCommand(..)
  , IdePlugins(..)
  -- * The IDE monad
  , IdeM
  , IdeState(..)
  , ExtensionClass(..)
  , ModuleCache(..)
  , getPlugins
  , runPluginCommand
  , pluginDescToIdePlugins
  , CachedModule(..)
  , getCachedModule
  , withCachedModule
  , withCachedModuleAndData
  , cacheModule
  , deleteCachedModule
  , oldRangeToNew
  , newRangeToOld
  , canonicalizeUri
  , getCradle
  , getTypecheckedModuleGhc
  , genLocMap
  , getIdsAtPos
  , LocMap
  -- * All the good types
  , module Haskell.Ide.Engine.PluginTypes
  ) where

import           Data.Aeson
import           Control.Monad.State.Strict
import           Data.Dynamic
import           Data.Maybe
import           Data.Monoid
import           Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import           Haskell.Ide.Engine.PluginTypes
import           Haskell.Ide.Engine.MonadFunctions
import qualified GhcMod.Monad as GM
import qualified GhcMod.Types as GM
import qualified GhcMod.Cradle as GM
import qualified GhcMod.Target as GM
import           GHC(TypecheckedModule)
import           System.Directory
import           System.FilePath
import           GHC.Generics

import qualified Data.IntervalMap.FingerTree as IM
-- import qualified GHC.SYB.Utils as SYB
import qualified Data.Generics as SYB
-- import qualified Language.Haskell.Refact.Utils.MonadFunctions as HaRe

import qualified GHC           as GHC
import qualified Var           as Var
import qualified SrcLoc        as GHC
import qualified GhcMonad      as GHC
import qualified Hooks         as GHC
import qualified HscMain       as GHC
import qualified HscTypes      as GHC
import qualified TcRnMonad     as GHC
import qualified DynFlags      as GHC

-- import StringBuffer

import Data.IORef

-- ---------------------------------------------------------------------

type PluginId = T.Text
type CommandName = T.Text

data CommandFunc a b = CmdSync (a -> IdeM (IdeResponse b))
                     | CmdAsync ((IdeResponse b -> IO ()) -> a -> IdeM ())

data PluginCommand = forall a b. (FromJSON a, ToJSON b) =>
  PluginCommand { commandName :: CommandName
                , commandDesc :: T.Text
                , commandFunc :: CommandFunc a b
                }

data PluginDescriptor =
  PluginDescriptor { pluginName :: T.Text
                   , pluginDesc :: T.Text
                   , pluginCommands :: [PluginCommand]
                   }

instance Show PluginCommand where
  show (PluginCommand name _ _) = "PluginCommand { name = " ++ T.unpack name ++ " }"

-- | Subset type extracted from 'Plugins' to be sent to the IDE as
-- a description of the available commands
newtype IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId [PluginCommand]
  } deriving (Show,Generic)

instance ToJSON IdePlugins where
  toJSON (IdePlugins m) = toJSON $ (fmap . fmap) (\x -> (commandName x, commandDesc x)) m

-- ---------------------------------------------------------------------
pluginDescToIdePlugins :: [(PluginId,PluginDescriptor)] -> IdePlugins
pluginDescToIdePlugins = IdePlugins . foldr (uncurry Map.insert . f) Map.empty
  where f = fmap pluginCommands

runPluginCommand :: PluginId -> CommandName -> Value -> (IdeResponse Value -> IO ()) -> IdeM ()
runPluginCommand p com arg callback = do
  let ret = liftIO . callback
  (IdePlugins m) <- getPlugins
  case Map.lookup p m of
    Nothing -> ret $
      IdeResponseFail $ IdeError UnknownPlugin ("Plugin " <> p <> " doesn't exist") Null
    Just xs -> case find ((com ==) . commandName) xs of
      Nothing -> ret $ IdeResponseFail $
        IdeError UnknownCommand ("Command " <> com <> " isn't defined for plugin " <> p <> ". Legal commands are: " <> T.pack(show $ map commandName xs)) Null
      Just (PluginCommand _ _ cf) -> case fromJSON arg of
        Error err -> ret $ IdeResponseFail $
          IdeError ParameterError ("error while parsing args for " <> com <> " in plugin " <> p <> ": " <> T.pack err) Null
        Success a -> case cf of
          CmdSync f -> do
            res <- f a
            ret $ fmap toJSON res
          CmdAsync f -> do
            f (callback . fmap toJSON) a
-- ---------------------------------------------------------------------

type IdeM = IdeT IO
type IdeT m = GM.GhcModT (StateT IdeState m)

data IdeState = IdeState
  {
    idePlugins :: IdePlugins
  , extensibleState :: !(Map.Map TypeRep Dynamic)
              -- ^ stores custom state information.
  , cradleCache :: !(Map.Map FilePath GM.Cradle)
              -- ^ map from dirs to cradles
  , uriCaches  :: !UriCaches
  } deriving (Show)

type UriCaches = Map.Map Uri UriCache

data UriCache = UriCache
  { cachedModule :: !CachedModule
  , cachedData   :: !(Map.Map TypeRep Dynamic)
  } deriving Show

type LocMap = IM.IntervalMap Position GHC.Name

data CachedModule = CachedModule
  { tcMod       :: !TypecheckedModule
  , locMap      :: !LocMap
  , revMap      :: !(FilePath -> FilePath)
  , newPosToOld :: !(Position -> Maybe Position)
  , oldPosToNew :: !(Position -> Maybe Position)
  }

instance Show CachedModule where
  show CachedModule{} = "CachedModule { .. }"

-- ---------------------------------------------------------------------

unpackRealSrcSpan :: GHC.RealSrcSpan -> (Position, Position)
unpackRealSrcSpan rspan =
  (toPos (l1,c1),toPos (l2,c2))
  where s  = GHC.realSrcSpanStart rspan
        l1 = GHC.srcLocLine s
        c1 = GHC.srcLocCol s
        e  = GHC.realSrcSpanEnd rspan
        l2 = GHC.srcLocLine e
        c2 = GHC.srcLocCol e

genLocMap :: TypecheckedModule -> LocMap
genLocMap tm = names
  where
    typechecked = GHC.tm_typechecked_source tm
    renamed = fromJust $ GHC.tm_renamed_source tm

    rspToInt = uncurry IM.Interval . unpackRealSrcSpan

    names  = IM.union names2 $ SYB.everything IM.union (IM.empty `SYB.mkQ` hsRecFieldT) typechecked
    names2 = SYB.everything IM.union (IM.empty `SYB.mkQ`  fieldOcc
                                               `SYB.extQ` hsRecFieldN
                                               `SYB.extQ` checker) renamed

    checker (GHC.L (GHC.RealSrcSpan r) x) = IM.singleton (rspToInt r) x
    checker _ = IM.empty

    fieldOcc :: GHC.FieldOcc GHC.Name -> LocMap
    fieldOcc (GHC.FieldOcc (GHC.L (GHC.RealSrcSpan r) _) n) = IM.singleton (rspToInt r) n
    fieldOcc _ = IM.empty

    hsRecFieldN :: GHC.LHsExpr GHC.Name -> LocMap
    hsRecFieldN (GHC.L _ (GHC.HsRecFld (GHC.Unambiguous (GHC.L (GHC.RealSrcSpan r) _) n) )) = IM.singleton (rspToInt r) n
    hsRecFieldN _ = IM.empty

    hsRecFieldT :: GHC.LHsExpr GHC.Id -> LocMap
    hsRecFieldT (GHC.L _ (GHC.HsRecFld (GHC.Ambiguous (GHC.L (GHC.RealSrcSpan r) _) n) )) = IM.singleton (rspToInt r) (Var.varName n)
    hsRecFieldT _ = IM.empty

getIdsAtPos ::Position -> LocMap -> [(Range, GHC.Name)]
getIdsAtPos p im = map f $ IM.search p im
  where f (IM.Interval a b, x) = (Range a b, x)
-- ---------------------------------------------------------------------

cachedModules :: IdeState -> Map.Map Uri CachedModule
cachedModules = fmap cachedModule . uriCaches

canonicalizeUri :: MonadIO m => Uri -> m Uri
canonicalizeUri uri =
  case uriToFilePath uri of
    Nothing -> return uri
    Just fp -> do
      fp' <- liftIO $ canonicalizePath fp
      return $ filePathToUri fp'

getCradle :: Uri -> IdeM GM.Cradle
getCradle uri =
  case uriToFilePath uri of
    Nothing -> do
      debugm $ "getCradle: malformed uri: " ++ show uri
      GM.cradle
    Just fp -> do
      dir <- liftIO $ takeDirectory <$> canonicalizePath fp
      mcradle <- lift . lift $ gets (Map.lookup dir . cradleCache)
      case mcradle of
        Just crdl -> do
          debugm $ "cradle cache hit for " ++ dir ++ ", using cradle " ++ show crdl
          return crdl
        Nothing -> do
          opts <- GM.options
          crdl <- GM.findCradle' (GM.optPrograms opts) dir
          debugm $ "cradle cache miss for " ++ dir ++ ", generating cradle " ++ show crdl
          lift . lift $ modify' (\s -> s { cradleCache = Map.insert dir crdl (cradleCache s)})
          return crdl

getCachedModule :: Uri -> IdeM (Maybe CachedModule)
getCachedModule uri = do
  uri' <- canonicalizeUri uri
  lift . lift $ gets (Map.lookup uri' . cachedModules)

withCachedModule :: Uri -> IdeM b -> (CachedModule -> IdeM b) -> IdeM b
withCachedModule uri noCache callback = do
  mcm <- getCachedModule uri
  case mcm of
    Nothing -> noCache
    Just cm -> callback cm

withCachedModuleAndData :: forall a b. ModuleCache a
  => Uri -> IdeM b -> (CachedModule -> a -> IdeM b) -> IdeM b
withCachedModuleAndData uri noCache callback = do
  uri' <- canonicalizeUri uri
  mc <- lift . lift $ gets (Map.lookup uri' . uriCaches)
  case mc of
    Nothing -> noCache
    Just UriCache{cachedModule = cm, cachedData = dat} -> do
      a <- case Map.lookup (typeRep $ Proxy @a) dat of
             Nothing -> do
               val <- cacheDataProducer cm
               let typ = typeOf val
               debugm $ "withCachedModuleAndData: Cache miss - " ++ show typ
               let dat' = Map.insert (typeOf val) (toDyn val) dat
               lift . lift $ modify' (\s -> s {uriCaches = Map.insert uri' (UriCache cm dat')
                                                                           (uriCaches s)})
               return val
             Just x -> do
               debugm $ "withCachedModuleAndData: Cache hit - " ++ show (typeRep $ Proxy @a)
               case fromDynamic x of
                 Just val -> return val
                 Nothing -> error "impossible"
      callback cm a


cacheModule :: Uri -> CachedModule -> IdeM ()
cacheModule uri cm = do
  uri' <- canonicalizeUri uri
  lift . lift $ modify' (\s -> s { uriCaches = Map.insert uri' (UriCache cm Map.empty)
                                                               (uriCaches s) })

deleteCachedModule :: Uri -> IdeM ()
deleteCachedModule uri = do
  uri' <- canonicalizeUri uri
  lift . lift $ modify' (\s -> s { uriCaches = Map.delete uri' (uriCaches s) })

newRangeToOld :: CachedModule -> Range -> Maybe Range
newRangeToOld cm (Range start end) = do
  start' <- newPosToOld cm start
  end'   <- newPosToOld cm end
  return (Range start' end')

oldRangeToNew :: CachedModule -> Range -> Maybe Range
oldRangeToNew cm (Range start end) = do
  start' <- oldPosToNew cm start
  end'   <- oldPosToNew cm end
  return (Range start' end')

getPlugins :: IdeM IdePlugins
getPlugins = lift $ lift $ idePlugins <$> get

-- ---------------------------------------------------------------------
-- Module Loading
-- ---------------------------------------------------------------------

type HookIORefData = Maybe TypecheckedModule

getMappedFileName :: FilePath -> GM.FileMappingMap -> FilePath
getMappedFileName fname mfs =
  case Map.lookup fname mfs of
    Just fm -> GM.fmPath fm
    Nothing -> fname

canonicalizeModSummary :: (MonadIO m) =>
  GHC.ModSummary -> m (Maybe FilePath)
canonicalizeModSummary =
  traverse (liftIO . canonicalizePath) . GHC.ml_hs_file . GHC.ms_location

tweakModSummaryDynFlags :: GHC.ModSummary -> GHC.ModSummary
tweakModSummaryDynFlags ms =
  let df = GHC.ms_hspp_opts ms
  in ms { GHC.ms_hspp_opts = GHC.gopt_set df GHC.Opt_KeepRawTokenStream }

getTypecheckedModuleGhc :: GM.IOish m
  => (GM.GmlT m () -> GM.GmlT m a) -> FilePath -> GM.GhcModT m (a, Maybe TypecheckedModule)
getTypecheckedModuleGhc wrapper targetFile = do
  cfileName <- liftIO $ canonicalizePath targetFile
  mfs <- GM.getMMappedFiles
  mFileName <- liftIO . canonicalizePath $ getMappedFileName cfileName mfs
  let ips = map takeDirectory $ Map.keys mfs
      setIncludePaths df = df { GHC.includePaths = ips ++ GHC.includePaths df }
  ref <- liftIO $ newIORef Nothing
  let
    setTarget fileName
      = GM.runGmlTWith' [Left fileName]
                        (return . setIncludePaths)
                        (Just $ updateHooks cfileName mFileName ref)
                        wrapper
                        (return ())
  res <- setTarget cfileName
  mtm <- liftIO $ readIORef ref
  return (res, mtm)

updateHooks :: FilePath -> FilePath -> IORef HookIORefData -> GHC.Hooks -> GHC.Hooks
updateHooks _ofp fp ref hooks = hooks {
        GHC.hscFrontendHook   = Just $ fmap GHC.FrontendTypecheck . hscFrontend fp ref
      }

-- discards all changes to Session
runGhcInHsc :: GHC.Ghc a -> GHC.Hsc a
runGhcInHsc action = do
  env <- GHC.getHscEnv
  session <- liftIO $ newIORef env
  liftIO $ GHC.reflectGhc action $ GHC.Session session

hscFrontend :: FilePath -> IORef HookIORefData -> GHC.ModSummary -> GHC.Hsc GHC.TcGblEnv
hscFrontend fn ref mod_summary = do
    mfn <- canonicalizeModSummary mod_summary
    let
      md = GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod mod_summary
      keepInfo = case mfn of
                   Just fileName -> fn == fileName
                   Nothing -> False
    liftIO $ debugm $ "hscFrontend: got mod,file" ++ show (md, mfn)
    if keepInfo
      then runGhcInHsc $ do
        let modSumWithRaw = tweakModSummaryDynFlags mod_summary

        p' <- GHC.parseModule modSumWithRaw
        let p = p' {GHC.pm_mod_summary = mod_summary}
        tc <- GHC.typecheckModule p
        let tc_gbl_env = fst $ GHC.tm_internals_ tc

        liftIO $ writeIORef ref $ Just tc
        return tc_gbl_env
      else do
        hpm <- GHC.hscParse' mod_summary
        hsc_env <- GHC.getHscEnv
        GHC.tcRnModule' hsc_env mod_summary False hpm

-- ---------------------------------------------------------------------
-- Extensible state, based on
-- http://xmonad.org/xmonad-docs/xmonad/XMonad-Core.html#t:ExtensionClass
--

-- | Every module must make the data it wants to store
-- an instance of this class.
--
-- Minimal complete definition: initialValue
class Typeable a => ExtensionClass a where
    -- | Defines an initial value for the state extension
    initialValue :: a

class Typeable a => ModuleCache a where
    -- | Defines an initial value for the state extension
    cacheDataProducer :: CachedModule -> IdeM a

instance ModuleCache () where
    cacheDataProducer = const $ return ()
