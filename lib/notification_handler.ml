(* open Jsonrpc *)
open Lsp
(* open Diagnostics *)
(* open Types *)
(* open Parse *)

let log_msg msg =
  let oc = open_out_gen [Open_append] 0o666 "/tmp/logs.log" in
  Printf.fprintf oc "%s\n" msg;
  close_out oc;;

let handle_notification notification =
  match Client_notification.of_jsonrpc notification with 
  | Ok _notif -> ()
  | Error _message -> ()

let on_notification (notification : Client_notification.t) =
  match notification with
  | Client_notification.TextDocumentDidOpen { textDocument = { text = _; _}} -> 
      let _diagnostic = Diagnostics.diagnostics_of_file "/home/drovosek/dip/lsp/jasmin-lsp/example.jazz" 
      (* Lwt.return ( Utils.send ( LSP_.notify diagnostic))  *)
    in ()
  | _ -> ()