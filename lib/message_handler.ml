open Jsonrpc
open Request_handler

let log_msg msg =
  let oc = open_out_gen [Open_append;Open_trunc;Open_creat] 0o666 "/tmp/logs.log" in
  Printf.fprintf oc "%s\n" msg;
  close_out oc;;

let handle_message (msg : Packet.t) =
  (* let a = Yojson.Safe.pretty_to_string @@ Packet.yojson_of_t msg in
  log_msg a; *)
  match msg with 
  | Request req -> log_msg "req"; on_request req
  | Notification _ -> log_msg "notification"; msg
  | _ -> log_msg "smth else"; msg