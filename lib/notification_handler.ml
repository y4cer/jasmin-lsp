(* open Jsonrpc *)
open Lsp
(* open Diagnostics *)
(* open Types *)
(* open Parse *)

let on_notification (_server : Rpc_server.t) (notification : Client_notification.t) =
  let packet =
    match notification with
    | Client_notification.TextDocumentDidOpen { textDocument = { text = _; uri; _}} -> 
        Logs.debug (fun m -> m "didopen");
        let diagnostic = Diagnostics.diagnostics_of_file uri in
        let packet = LSP_.notify diagnostic
      in Some packet
    | Client_notification.Initialized -> 
      Logs.debug (fun m -> m "initialized");
      None
    | _ -> None
  in _server, packet

let handle_notification (server : Rpc_server.t) notification =
  Logs.debug (
    fun m -> m ("Handling notification")
  );
  match Client_notification.of_jsonrpc notification with 
  | Ok notif -> on_notification server notif
  | Error _message -> server, None
