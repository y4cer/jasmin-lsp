open Jasmin_lsp.Rpc
open Jasmin_lsp.Message_handler
open Jsonrpc

let read () = RPC_IO.read Lwt_io.stdin

let write packet = RPC_IO.write Lwt_io.stdout packet

let flush () = Lwt_io.flush Lwt_io.stdout

let send packet = 
  let%lwt () = write packet in
  flush ()

let () = 
  let _rpc_loop = Lwt_main.run (
    let%lwt client_msg = read () in
    match client_msg with
    | Some msg ->
      let res = handle_message msg in
      log_msg ( Yojson.Safe.pretty_to_string @@ Packet.yojson_of_t res);
      log_msg "132";
      send res;
    | None ->
      log_msg "Stop";
      Lwt.return_unit
  )
  in ()
