{-# LANGUAGE OverloadedStrings #-}
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Control.Monad.IO.Class
import Control.Applicative.Combinators
import Control.Concurrent
import Control.Monad
import Control.Concurrent


hieCommand :: String
hieCommand = "./dist-newstyle/build/x86_64-linux/ghc-8.6.5/haskell-ide-engine-0.11.0.0/x/hie/build/hie/hie -d +RTS -hy -l"

main = runSessionWithConfig (defaultConfig { logStdErr = True, logMessages = False, messageTimeout = 500 }) hieCommand fullCaps "/home/zubin/haskell-ide-engine/test/testdata/completion/" $ do
  doc <- openDoc "Completion.hs" "haskell"
  waitForDiagnostics
  waitForDiagnostics
  replicateM 20 $ do
    liftIO $ putStrLn "----editing----"
    let te = TextEdit (Range (Position 5 7) (Position 5 24)) "putStrLn \"\"\n"
    _ <- applyEdit doc te
    liftIO $ threadDelay $ 2*10^6
    sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
    waitForDiagnostics
    waitForDiagnostics
  replicateM 20 $ do
    liftIO $ putStrLn "----editing----"
    let te = TextEdit (Range (Position 5 7) (Position 5 24)) "putStrLn \"\"\n"
    _ <- applyEdit doc te
    liftIO $ threadDelay $ 2*10^6
    sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
    waitForDiagnostics
    waitForDiagnostics
    compls <- getCompletions doc (Position 5 9)
    liftIO $ print compls
  sendNotification Exit (Nothing :: Maybe Char)
  liftIO $ threadDelay $ 1*10^6

