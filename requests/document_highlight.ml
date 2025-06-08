open Lsp
open Types
open Jasmin_lsp
open Parse

let highlight (params : DocumentHighlightParams.t) = 
  let fname = params.textDocument.uri in
  let pos = Position.create ~line:(params.position.line + 1) ~character:(params.position.character) in
  let res = parse_file (Some pos) (Uri.to_path fname) in

  let start_pos = Position.create ~line:2 ~character:10 in
  let end_pos = Position.create ~line:2 ~character:10  in
  let range = Range.create ~start:start_pos ~end_:end_pos in
  let _h = DocumentHighlight.create ~range ~kind:DocumentHighlightKind.Text () in

  let start_pos_ = Position.create ~line:3 ~character:6 in
  let end_pos_ = Position.create ~line:3 ~character:6  in
  let range_ = Range.create ~start:start_pos_ ~end_:end_pos_ in
  let _h_ = DocumentHighlight.create ~range:range_ ~kind:DocumentHighlightKind.Text () in

  let start_pos_ = Position.create ~line:6 ~character:19 in
  let end_pos_ = Position.create ~line:6 ~character:19  in
  let range_ = Range.create ~start:start_pos_ ~end_:end_pos_ in
  let _h__ = DocumentHighlight.create ~range:range_ ~kind:DocumentHighlightKind.Text () in

  match res with
  (* | Ok _ -> [h; h_; h__] *)
  | Ok _ -> []
  | Error _ -> 
    Logs.err (fun m -> m "error happened");
    []