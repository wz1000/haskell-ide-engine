{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Haskell.Ide.ExamplePlugin2 where

import           Haskell.Ide.Engine.PluginDescriptor
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text as T

-- ---------------------------------------------------------------------

example2Descriptor :: PluginDescriptor
example2Descriptor = PluginDescriptor
  {
    pluginName = "Hello World"
  , pluginDesc = "An example of writing an HIE plugin"
  , pluginCommands =
      [ PluginCommand "sayHello" "say hello" sayHelloCmd
      , PluginCommand "sayHelloTo ""say hello to the passed in param" sayHelloToCmd
      ]
  }

-- ---------------------------------------------------------------------

sayHelloCmd :: CommandFunc () T.Text
sayHelloCmd = CmdSync $ \_ -> return (IdeResponseOk sayHello)

sayHelloToCmd :: CommandFunc T.Text T.Text
sayHelloToCmd = CmdSync $ \n -> do
  r <- liftIO $ sayHelloTo n
  return $ IdeResponseOk r

-- ---------------------------------------------------------------------

sayHello :: T.Text
sayHello = "hello from ExamplePlugin2"

sayHelloTo :: T.Text -> IO T.Text
sayHelloTo n = return $ "hello " <> n <> " from ExamplePlugin2"
