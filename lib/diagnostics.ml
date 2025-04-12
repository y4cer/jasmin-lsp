(* open Jsonrpc *)
open Lsp
open Types
(* open Parse *)

module SN = Server_notification

let log_msg msg =
  let oc = open_out_gen [Open_append] 0o666 "/tmp/logs.log" in
  Printf.fprintf oc "%s\n" msg;
  close_out oc;;

(*TODO: think about how to get parsing error. Parsing error is not actually an error in our case, it is desirable*)
(* let range_of_err err =  *)

let diagnostic =
  Diagnostic.create
    ~range:(Range.create 
              ~start: (Position.create ~line: 1 ~character: 1)
              ~end_: (Position.create ~line: 1 ~character: 3)
            )
    ~message: "test_message" 
    ~severity: DiagnosticSeverity.Error

let diagnostics_of_file file =
  let diagnostics = diagnostic () in
  let params = 
    PublishDiagnosticsParams.create
      ~uri:(Uri.of_path file)
      ~diagnostics: [diagnostics]
      ()
  in
  SN.PublishDiagnostics params