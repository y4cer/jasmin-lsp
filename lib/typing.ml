open Jasmin

let type_program fname = 
  let module C = (val Jasmin.CoreArchFactory.core_arch_x86 ~use_lea:true ~use_set0:true !Jasmin.Glob_options.call_conv) in
  let module Arch = Jasmin.Arch_full.Arch_from_Core_arch (C) in
  Logs.debug (fun m -> m "%s" fname);
  let _env, _pprog, _ast =
    try Compile.parse_file Arch.arch_info ~idirs:!Glob_options.idirs fname
    with
    | Annot.AnnotationError (_loc, code) -> 
      Logs.debug (fun m -> m "annotation error %t" code);
      raise (End_of_file)
    | Pretyping.TyError (_loc, code) ->
      Logs.debug (fun m -> m "typing error %a" Pretyping.pp_tyerror code);
      raise (End_of_file)
    | Syntax.ParseError (_loc, msg) ->
        let msg =
          match msg with
          | None -> "unexpected token" (* default message *)
          | Some msg -> msg
        in
        Logs.debug (fun m -> m "parse error %s" msg);
        raise (End_of_file)
  in ()