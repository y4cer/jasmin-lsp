open Jasmin_lsp
open Lsp
open Types
open Rpc_server

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

let notify_and_log_error_sync msg e =
  let exn_str = Printexc.to_string e in
  Lwt.dont_wait
    (fun () -> Utils.send @@ LSP_.notify_show_message ~kind:MessageType.Error exn_str)
    (fun exn ->
      Logs.err (fun m -> m "%s: %s" msg (Printexc.to_string exn));
      let backtrace = Printexc.get_backtrace () in
      Logs.err (fun m -> m "Backtrace: %s" backtrace))

let start (server : Rpc_server.t) =
  let rec rpc_loop server =
    match server.state with
    | State.Stopped -> 
      Logs.info (fun m -> m "Server stopped");
      Lwt.return_unit;
    | _ -> (
      let%lwt client_msg = Utils.read () in
      match client_msg with
      | Some msg -> (
        let%lwt server = 
          let server, reply_result = Message_handler.handle_message server msg
        in Lwt.dont_wait
          (fun () -> Utils.send reply_result)
          (notify_and_log_error_sync "Error");
        Lwt.return server
      in rpc_loop server
      )
      | None -> 
        Logs.debug (fun m -> m "Client disconnected");
        Lwt.return_unit
    )
    in rpc_loop server

let () =
  setup_logging_to_file "/tmp/logs.log";
  let server = { custom = "123"; state = State.Uninitialized } in
  Lwt_main.run @@ start server