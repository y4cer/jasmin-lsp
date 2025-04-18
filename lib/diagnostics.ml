open Lsp
open Types
open Parse

module SN = Server_notification

let diagnostic (loc : Lexer.L.t) msg =
  Diagnostic.create
    ~range:(Range.create 
              ~start: (Position.create ~line: (fst loc.loc_start - 1) ~character: (snd loc.loc_start))
              ~end_: (Position.create ~line: (fst loc.loc_end - 1) ~character: (snd loc.loc_end))
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