open Jasmin_lsp

let () = 
  let _rpc_loop = Lwt_main.run (
    let%lwt client_msg = Utils.read () in
    match client_msg with
    | Some msg ->
      let res = Message_handler.handle_message msg in
      Utils.send res;
    | None ->
      Lwt.return_unit
  )
  in ()
