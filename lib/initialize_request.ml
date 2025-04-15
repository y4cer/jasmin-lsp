open Lsp
open Types
open Rpc_server

let on_request (server : Rpc_server.t) (_params : InitializeParams.t) =
  let capabilities = 
    ServerCapabilities.create
    ~textDocumentSync:
      ( `TextDocumentSyncOptions
        (TextDocumentSyncOptions.create
          ~openClose:true
          ~change:TextDocumentSyncKind.Full
          (* ~willSave:false
          ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
          ~willSaveWaitUntil:false *)
          ()
        )
      )
    ~workspace:
      (ServerCapabilities.create_workspace
         ~workspaceFolders:
           (WorkspaceFoldersServerCapabilities.create ~supported:true
              ~changeNotifications:(`Bool true) ())
         ())
    ~hoverProvider:(`Bool true) ~codeActionProvider:(`Bool true)
    ()
  in
  let serverInfo =
    InitializeResult.create_serverInfo ~name: "jasmin-lsp" ~version:"0.0.0" () in
  let init = 
    InitializeResult.create ~capabilities ~serverInfo () in
  let server = 
    {
      custom = server.custom;
      state = Rpc_server.State.Running
    }
  in server, init
