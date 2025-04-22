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

type reg = 
  ( X86_decl.register,
    X86_decl.register_ext,
    X86_decl.xmm_register,
    X86_decl.rflag,
    X86_decl.condt,
    X86_instr_decl.x86_op,
    X86_extra.x86_extra_op )
  Arch_extra.extended_op

type result = Ok of (unit, reg) pmod_item list * Lexer.S.pprogram | Error of Lexer.L.t * string
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

(* let iter_fbody (f_body : ((pexpr_, unit, reg) gstmt)) =
() *)

let iter_pprog (pprog_item : ('info, 'asm) pmod_item) = 
  let _  = match pprog_item with
  | MIfun {f_name; f_body; f_tyout; _} ->
    let _ret_ty = List.map f_tyout ~f:(
      fun (ty : pexpr_ gty) -> (
        let str_type = Format.asprintf "%a" Printer.pp_ptype ty in
        Logs.debug (fun m -> m "%s" str_type);
      )
    ) in
    Logs.debug (fun m -> m "%s" f_name.fn_name);
    Logs.debug (fun m -> m "%d" @@ List.length f_body);
    (* List.iter f_body ~f:iter_fbody; *)
  | _ -> () 
  in 
  ()

(*
NB: do not forget about name shadowing. 
e.g.

export
fn forty() -> reg u64 {
  reg u32 r = 0;            <- r declared first time
  if (r == 0) {
    reg u64 r = 1;          <- r declared second time
  }
  reg u64 r = (64u)r;       <- r declared third time
  return r;                 <- if I hover over this r, 
                               only the one above must be highlighted
}

*)
(* let find_enclosing_interval loc ast = () *)

let parse_file (fname : string) : result = 
  let module C = (val Jasmin.CoreArchFactory.core_arch_x86 ~use_lea:true ~use_set0:true !Jasmin.Glob_options.call_conv) in
  let module Arch = Jasmin.Arch_full.Arch_from_Core_arch (C) in
  Logs.debug (fun m -> m "filename: %s" fname);
  try (
    let _env, pprog, ast = Compile.parse_file Arch.arch_info ~idirs:!Glob_options.idirs fname in
    let _ = List.iter ~f:iter_pprog pprog in
    let deps = Pretyping.Env.dependencies _env in
    let _func = Pretyping.Env.Funs.find "forty" _env in
    List.iter deps ~f:(fun dep -> Logs.debug (fun m -> m "%s" @@ String.concat dep ~sep:" "));
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