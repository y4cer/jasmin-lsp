open Jsonrpc
open Jasmin_lsp.Rpc
(* open Lsp *)

let log_msg msg =
  let oc = open_out_gen [Open_trunc; Open_append] 0o600 "/tmp/logs.log" in
  Printf.fprintf oc "%s\n" msg;
  close_out oc;;

let serialize_request_example id =
  (* Create a simple request to call a "subtract" method with two parameters *)

  let request = Jsonrpc.Request.create 
    ~id:(`String id)
    ~method_:"subtract" 
    () in
  
  (* Serialize the request to a JSON string *)
  let json_str = Jsonrpc.Request.yojson_of_t request |> Yojson.Safe.to_string in
  log_msg json_str;

  (json_str)

let _ = serialize_request_example


let handle_message (msg : Packet.t) =
  let a = Yojson.Safe.pretty_to_string @@ Packet.yojson_of_t msg in
  log_msg a;
  ()

let read () = RPC_IO.read Lwt_io.stdin

let () = 
  let _rpc_loop = Lwt_main.run (
    let%lwt client_msg = read () in
    match client_msg with
    | Some msg ->
      handle_message msg;
      Lwt.return_unit
    | None ->
      log_msg "aboba";
      Lwt.return_unit
  )
  in ()
