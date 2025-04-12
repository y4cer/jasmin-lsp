open Lsp
open Types

let on_request (_params : InitializeParams.t) =
  let capabilities = 
    ServerCapabilities.create
      ~textDocumentSync:
        (`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create ~openClose:true 
          ~change: TextDocumentSyncKind.Full ~save:(`Bool true) ())) ()
  in
  let init = 
    InitializeResult.
      { 
        capabilities = capabilities;
        serverInfo = Some { name = "jasmin-lsp"; version = Some "0.0.1" }
      }
  in init
