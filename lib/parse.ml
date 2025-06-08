open Core
open Jasmin
open Jasmin.Prog
open Lsp.Types

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

(* TODO: incremental parsing *)

(* NB: node.d_loc might be used for goto definition! *)

let check_if_pos_in_node (pos: Position.t) (loc: Prog.L.t) =
  let p1_bol = loc.loc_bchar - (snd loc.loc_start) in
  (pos.line >= fst loc.loc_start) &&
  (pos.line <= fst loc.loc_end)   &&
  (pos.character + p1_bol >= loc.loc_bchar) &&
  (pos.character + p1_bol <= loc.loc_echar)

let return_if_pos (type a) (pos: Position.t) (item: a L.located) : a Prog.L.located option =
  if (check_if_pos_in_node pos item.pl_loc) then
    Some item
  else None 

let iter_var (type a) (_pos: Position.t) (_expr: pexpr_ gvar) : a Prog.L.located option = 
  None

let iter_expr (type a) (_pos: Position.t) (_expr: pexpr_ gexpr) : a Prog.L.located option = 
  None

let iter_instr (type a) (pos: Position.t) (instr : ((pexpr_, unit, reg) ginstr)) : a Prog.L.located option =
  match instr.i_desc with
  | Cassgn (glval, _assign_tag, _gty, gexpr) -> 
    (let _ = match glval with 
      | Lvar gvar -> 
        ( let var_id = gvar.pl_desc.v_id in
          let line = fst gvar.pl_loc.loc_start in
          let _aa = return_if_pos pos gvar in
          Logs.debug (fun m -> m "line: %d, name: %s, id: %s" line gvar.pl_desc.v_name (Prog.string_of_uid var_id));
          let _ = iter_expr pos gexpr in
        ()
        )
      | _ -> ()
    in
    let str_type = Format.asprintf "%a" Printer.pp_pexpr gexpr in
    Logs.debug (fun m -> m "%s" str_type);
    None
    )
  | _ -> 
    Logs.debug (fun m -> m "unknown");
    None

let iter_pprog (pos: Position.t option) (pprog_item : ('info, 'asm) pmod_item) = 
  match pos with 
  | None -> ()
  | Some pos ->
    let _  = match pprog_item with
    | MIfun {f_name = _; f_body; f_tyout; _} ->
      let _ret_ty = List.map f_tyout ~f:(
        fun (ty : pexpr_ gty) -> (
          let str_type = Format.asprintf "%a" Printer.pp_ptype ty in
          Logs.debug (fun m -> m "%s" str_type);
        )
      ) in
      (* Logs.debug (fun m -> m "%s" f_name.fn_name);
      Logs.debug (fun m -> m "%d" @@ List.length f_body); *)
      List.iter f_body ~f:(fun instr -> (
        let base_loc = instr.i_loc.base_loc in
        if (check_if_pos_in_node pos base_loc) then 
          let _ = iter_instr pos instr 
          in ()
        else () 
      ));
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

let parse_file (pos: Position.t option) (fname : string) : result = 
  let module C = (val Jasmin.CoreArchFactory.core_arch_x86 ~use_lea:true ~use_set0:true !Jasmin.Glob_options.call_conv) in
  let module Arch = Jasmin.Arch_full.Arch_from_Core_arch (C) in
  Logs.debug (fun m -> m "filename: %s" fname);
  try (
    let _env, pprog, ast = Compile.parse_file Arch.arch_info ~idirs:!Glob_options.idirs fname in
    let _ = List.iter ~f:(iter_pprog pos) pprog in
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