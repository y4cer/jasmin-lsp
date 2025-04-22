open Core
open Lexing
open Jasmin
open Jasmin.Prog

module P = Jasmin.Parser

type error_position = 
  {
    start_p: Lexing.position;
    end_p: Lexing.position;
  }

type pprogram = ( unit,
  ( X86_decl.register,
    X86_decl.register_ext,
    X86_decl.xmm_register,
    X86_decl.rflag,
    X86_decl.condt,
    X86_instr_decl.x86_op,
    X86_extra.x86_extra_op )
  Arch_extra.extended_op )
pmod_item
list

type result = Ok of pprogram * Lexer.S.pprogram | Error of Lexer.L.t * string
let pos_of_lexbuf lexbuf : Lexer.L.t =
  let start_p = Lexing.lexeme_start_p lexbuf in
  let end_p = Lexing.lexeme_end_p lexbuf in
  Lexer.L.make start_p end_p

let string_of_position (pos : Lexing.position) =
  Printf.sprintf "%d:%d" pos.pos_lnum pos.pos_cnum

(* TODO: incremental parsing *)

(* Prints the line number and character number where the error occurred.*)
(* for debug only *)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let string_of_gty (gty : pexpr_ gty) =
  match gty with 
  | Bty (base_type : base_ty) -> 
    (match base_type with 
      | Bool -> "bool"
      | Int -> "int"
      | U wsize -> Format.sprintf "u%d" @@ int_of_ws wsize
    )
  | Arr (wsize, _len) -> Format.sprintf "u%d[NOT IMPLEMENTED]]" (*FIXME: add compile-time evaluation*) (int_of_ws wsize) 

let iter_pprog (pprog_item : ('info, 'asm) pmod_item) = 
  let _  = match pprog_item with
  | MIfun {f_name; f_body; f_tyout; _} ->
    let _ret_ty = List.map f_tyout ~f:(
      fun (ty : pexpr_ gty) -> (
        let str_type = string_of_gty ty in
        Logs.debug (fun m -> m "%s" str_type);
      )
    ) in
    Logs.debug (fun m -> m "%s" f_name.fn_name);
    Logs.debug (fun m -> m "%d" @@ List.length f_body);
  | _ -> () 
  in 
  ()

let parse_file (fname : string) : result = 
  let module C = (val Jasmin.CoreArchFactory.core_arch_x86 ~use_lea:true ~use_set0:true !Jasmin.Glob_options.call_conv) in
  let module Arch = Jasmin.Arch_full.Arch_from_Core_arch (C) in
  Logs.debug (fun m -> m "filename: %s" fname);
  try (
    let _env, pprog, ast = Compile.parse_file Arch.arch_info ~idirs:!Glob_options.idirs fname in
    let _ = List.iter ~f:iter_pprog pprog in
    Ok (pprog, ast)
  )
  with
    | Annot.AnnotationError (loc, code) -> 
      let msg = Format.asprintf "annotation error: %t" code in
      Logs.debug (fun m -> m "%s" msg);
      Error (loc, msg) 
    | Pretyping.TyError (loc, code) ->
      let msg = Format.asprintf "typing error: %a" Pretyping.pp_tyerror code in
      Logs.debug (fun m -> m "%s" msg);
      Error (loc, msg)
    | Syntax.ParseError (loc, msg') ->
        let msg =
          match msg' with
          | None -> "unexpected token" (* default message *)
          | Some msg -> msg
        in
        let msg = Format.asprintf "parse error: %s" msg in
        Logs.debug (fun m -> m "%s" msg);
        Error (loc, msg)