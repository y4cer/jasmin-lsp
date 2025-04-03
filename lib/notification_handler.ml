open Jsonrpc
(* open Lsp *)
(* open Types *)

let log_msg msg =
  let oc = open_out_gen [Open_append] 0o666 "/tmp/logs.log" in
  Printf.fprintf oc "%s\n" msg;
  close_out oc;;

let on_notification (notification : Notification.t) =
  match notification with 
  (* | Client_notification.TextDocumentDidOpen  *)
  | _ -> ()