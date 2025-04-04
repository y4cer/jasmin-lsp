open Jsonrpc
open Request_handler
open Notification_handler

let log_msg msg =
  let oc = open_out_gen [Open_append;Open_trunc;Open_creat] 0o666 "/tmp/logs.log" in
  Printf.fprintf oc "%s\n" msg;
  close_out oc;;

let handle_message (msg : Packet.t) =
  (* let a = Yojson.Safe.pretty_to_string @@ Packet.yojson_of_t msg in
  log_msg a; *)
  match msg with 
  | Request req -> log_msg (string_of_int @@ Random.int 10); on_request req
  | Notification notification -> log_msg "notification"; handle_notification notification; msg
  | _ -> log_msg "smth else"; msg