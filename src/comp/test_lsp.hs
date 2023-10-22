{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test
main = runSession "bsc_lsp" fullCaps "testsuite/lsp" $ do
  doc <- openDoc "Test1.bsv" "bsv"

  -- Use your favourite combinators.
  skipManyTill loggingNotification (count 1 publishDiagnosticsNotification)

  -- Send requests and notifications and receive responses
  rsp <-
    request SMethod_TextDocumentDocumentSymbol $
      DocumentSymbolParams Nothing Nothing doc
  liftIO $ print rsp

  -- Or use one of the helper functions
  getDocumentSymbols doc >>= liftIO . print
