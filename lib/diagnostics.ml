open Lsp
open Types
open Parse

module SN = Server_notification

let diagnostic loc msg =
  Diagnostic.create
    ~range:(Range.create 
              ~start: (Position.create ~line: loc.loc_lnum ~character: loc.loc_cnum)
              ~end_: (Position.create ~line: loc.loc_lnum ~character: loc.loc_cnum)
            )
    ~message: msg
    ~severity: DiagnosticSeverity.Error
    ()

let diagnostics_of_file file text =
  let res = parse_string text in
  let diagnostics = 
    match res with 
    | Ok _ -> []
    | Error (loc, msg) -> (
      let diag = diagnostic loc msg
      in [diag]
    )
  in
  let params = 
    PublishDiagnosticsParams.create
      ~uri: file
      ~diagnostics
      ()
  in
  SN.PublishDiagnostics params