open Jasmin_lsp.Rpc
open Jasmin_lsp.Message_handler

let read () = RPC_IO.read Lwt_io.stdin

let () = 
  let _rpc_loop = Lwt_main.run (
    let%lwt client_msg = read () in
    match client_msg with
    | Some msg ->
      log_msg @@ handle_message msg;
      Lwt.return_unit
    | None ->
      log_msg "Stop";
      Lwt.return_unit
  )
  in ()
