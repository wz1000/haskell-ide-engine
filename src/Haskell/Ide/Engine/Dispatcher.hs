{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.Engine.Dispatcher where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.Map as Map
import qualified Data.Set as S
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import qualified GhcMod.Monad.Env as GM
import qualified GhcMod.Monad.Types as GM
import qualified GhcMod.Types as GM
import System.Directory


data DispatcherEnv = DispatcherEnv
  { cancelReqsTVar :: TVar (S.Set J.LspId)
  , wipReqsTVar    :: TVar (S.Set J.LspId)
  , docVersionTVar :: TVar (Map.Map Uri Int)
  }

withCradle :: GM.Cradle -> IdeM a -> IdeM a
withCradle crdl =
  GM.gmeLocal (\env -> env {GM.gmCradle = crdl})

runActionWithContext :: Maybe Uri -> IdeM a -> IdeM a
runActionWithContext Nothing action = do
  crdl <- GM.cradle
  liftIO $ setCurrentDirectory $ GM.cradleRootDir crdl
  action
runActionWithContext (Just uri) action = do
  crdl <- getCradle uri
  liftIO $ setCurrentDirectory $ GM.cradleRootDir crdl
  withCradle crdl action

dispatcherP :: DispatcherEnv -> TChan PluginRequest -> IdeM ()
dispatcherP DispatcherEnv{..} pin = forever $ do
  debugm "dispatcherP: top of loop"
  (PReq context mver mid callback action) <- liftIO $ atomically $ readTChan pin
  debugm $ "got request with id: " ++ show mid
  case mid of
    Nothing -> case mver of
      Nothing -> runActionWithContext context action >>= liftIO . callback
      Just (uri, reqver) -> do
        curver <- liftIO $ atomically $ Map.lookup uri <$> readTVar docVersionTVar
        if Just reqver /= curver then
          debugm "not processing request as it is for old version"
        else do
          debugm "Processing request as version matches"
          runActionWithContext context action >>= liftIO . callback
    Just lid -> do
      cancelReqs <- liftIO $ atomically $ do
        modifyTVar' wipReqsTVar (S.delete lid)
        readTVar cancelReqsTVar
      if S.member lid cancelReqs
        then do
          debugm $ "cancelling request: " ++ show lid
          liftIO $ atomically $ modifyTVar' cancelReqsTVar (S.delete lid)
        else do
          debugm $ "processing request: " ++ show lid
          runActionWithContext context action >>= liftIO . callback

