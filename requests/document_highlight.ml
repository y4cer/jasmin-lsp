open Lsp
open Types
open Jasmin_lsp
open Parse

let highlight (params : DocumentHighlightParams.t) = 
  let fname = params.textDocument.uri in
  let res = parse_file (Uri.to_path fname) in
  let _pos = params.position in
  let start_pos = Position.create ~line:1 ~character:1 in
  let end_pos = Position.create ~line:1 ~character:4  in
  let range = Range.create ~start:start_pos ~end_:end_pos in
  let h = DocumentHighlight.create ~range ~kind:DocumentHighlightKind.Text () in
  match res with
  | Ok _ -> h
  | Error _ -> 
    Logs.err (fun m -> m "error happened");
    h