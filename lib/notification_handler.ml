(* open Jsonrpc *)
open Lsp
(* open Diagnostics *)
(* open Types *)
(* open Parse *)

let on_notification (notification : Client_notification.t) =
  match notification with
  | Client_notification.TextDocumentDidOpen { textDocument = { text = _; uri; _}} -> 
      Logs.debug (fun m -> m "didopen");
      let diagnostic = Diagnostics.diagnostics_of_file (Uri.to_string uri) in
      let packet = LSP_.notify diagnostic
    in Some packet
  | _ -> None

let handle_notification notification =
  Logs.debug (
    fun m -> m ("Handling notification")
  );
  match Client_notification.of_jsonrpc notification with 
  | Ok notif -> on_notification notif
  | Error _message -> None
