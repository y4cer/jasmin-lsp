open Lsp
open Types
open Jasmin_lsp
open Parse

let highlight (params : DocumentHighlightParams.t) = 
  let fname = params.textDocument.uri in
  let pos = Position.create ~line:(params.position.line + 1) ~character:(params.position.character) in
  let res = parse_file (Some pos) (Uri.to_path fname) in

  let start_pos = Position.create ~line:1 ~character:1 in
  let end_pos = Position.create ~line:1 ~character:4  in
  let range = Range.create ~start:start_pos ~end_:end_pos in
  let h = DocumentHighlight.create ~range ~kind:DocumentHighlightKind.Text () in

  let start_pos_ = Position.create ~line:2 ~character:1 in
  let end_pos_ = Position.create ~line:2 ~character:4  in
  let range_ = Range.create ~start:start_pos_ ~end_:end_pos_ in
  let h_ = DocumentHighlight.create ~range:range_ ~kind:DocumentHighlightKind.Text () in

  match res with
  | Ok _ -> [h; h_]
  | Error _ -> 
    Logs.err (fun m -> m "error happened");
    [h]