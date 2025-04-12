open Jasmin_lsp

let setup_logging_to_file filename =
  (* Create a formatter that writes to the file *)
  let file_formatter = 
    let oc = open_out_gen [Open_append;Open_trunc;Open_creat] 0o666 filename in
    Format.formatter_of_out_channel oc
  in
  
  (* Create a reporter that writes to the file *)
  let reporter = Logs_fmt.reporter ~dst:file_formatter () in
  
  (* Set the reporter and default log level *)
  Logs.set_reporter reporter;
  Logs.set_level (Some Logs.Debug);
  ()

let () = 
  setup_logging_to_file "/tmp/llogs.log";
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
