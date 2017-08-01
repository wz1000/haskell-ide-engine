{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Haskell.Ide.Engine.PluginUtils
  (
    mapEithers
  , pluginGetFile
  , diffText
  , srcSpan2Range
  , srcSpan2Loc
  , reverseMapFile
  , fileInfo
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import qualified Data.HashMap.Strict                   as H
import           Data.Monoid
import qualified Data.Text                             as T
import           FastString
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import           Prelude                               hiding (log)
import           SrcLoc
import           System.Directory
import           System.FilePath

-- ---------------------------------------------------------------------

getRealSrcSpan :: SrcSpan -> Either T.Text RealSrcSpan
getRealSrcSpan (RealSrcSpan r) = pure r
getRealSrcSpan (UnhelpfulSpan x) = Left $ T.pack $ unpackFS x

realSrcSpan2Range :: RealSrcSpan -> Range
realSrcSpan2Range rspan =
  Range (toPos (l1,c1)) (toPos (l2,c2))
  where s = realSrcSpanStart rspan
        l1 = srcLocLine s
        c1 = srcLocCol s
        e = realSrcSpanEnd rspan
        l2 = srcLocLine e
        c2 = srcLocCol e

srcSpan2Range :: SrcSpan -> Either T.Text Range
srcSpan2Range spn =
  realSrcSpan2Range <$> getRealSrcSpan spn

reverseMapFile :: MonadIO m => (FilePath -> FilePath) -> FilePath -> m FilePath
reverseMapFile rfm fp =
  liftIO $ canonicalizePath . rfm =<< canonicalizePath fp

srcSpan2Loc :: (MonadIO m) => (FilePath -> FilePath) -> SrcSpan -> m (Either T.Text Location)
srcSpan2Loc revMapp spn = runEitherT $ do
  rspan <- hoistEither $ getRealSrcSpan spn
  let fp = unpackFS $ srcSpanFile rspan
  file <- reverseMapFile revMapp fp
  return $ Location (filePathToUri file) (realSrcSpan2Range rspan)

-- ---------------------------------------------------------------------
pluginGetFile
  :: Monad m
  => T.Text -> Uri -> (FilePath -> m (IdeResponse a)) -> m (IdeResponse a)
pluginGetFile name uri f =
  case uriToFilePath uri of
    Just file -> f file
    Nothing -> return $ IdeResponseFail (IdeError PluginError
                 (name <> "Couldn't resolve uri" <> getUri uri) Null)

----------------------------------------

-- ---------------------------------------------------------------------
-- courtesy of http://stackoverflow.com/questions/19891061/mapeithers-function-in-haskell
mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y  -> Right (y:ys)
mapEithers _ _ = Right []

-- ---------------------------------------------------------------------

-- |Generate a 'WorkspaceEdit' value from a pair of source Text
diffText :: (Uri,T.Text) -> T.Text -> WorkspaceEdit
diffText (f,fText) f2Text = WorkspaceEdit (Just h) Nothing
  where
    d = getGroupedDiff (lines $ T.unpack fText) (lines $ T.unpack f2Text)
    diffOps = diffToLineRanges d
    r = map diffOperationToTextEdit diffOps
    diff = J.List r
    h = H.singleton f diff

    diffOperationToTextEdit :: DiffOperation LineRange -> J.TextEdit
    diffOperationToTextEdit (Change fm to) = J.TextEdit range nt
      where
        range = calcRange fm
        nt = T.pack $ init $ unlines $ lrContents to

    diffOperationToTextEdit (Deletion fm _) = J.TextEdit range ""
      where
        range = calcRange fm

    diffOperationToTextEdit (Addition fm _) = J.TextEdit range nt
      where
        range = calcRange fm
        nt = T.pack $ unlines $ lrContents fm


    calcRange fm = J.Range s e
      where
        sl = fst $ lrNumbers fm
        sc = 0
        s = J.Position (sl - 1) sc -- Note: zero-based lines
        el = snd $ lrNumbers fm
        ec = length $ last $ lrContents fm
        e = J.Position (el - 1) ec  -- Note: zero-based lines

-- ---------------------------------------------------------------------

-- | Returns the directory and file name
fileInfo :: T.Text -> (FilePath,FilePath)
fileInfo tfileName =
  let sfileName = T.unpack tfileName
      dir = takeDirectory sfileName
  in (dir,sfileName)
