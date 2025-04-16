open Lsp
open Types
open Jasmin_lsp
open Parse

let highlight (params : DocumentHighlightParams.t) = 
  (* let code = "export\nfn forty() -> reg u32 {\n  reg u32 r = 0;\n  return r;\n}\n" in *)
  let code = "param int cROUNDS = 2;\nparam int dROUNDS = 4;\n\ninline\nfn sipround(reg u64[4] v) -> reg u64[4] {\n  v[0] += v[1];\n  return v;\n}\n\nexport\nfn siphash_jazz(\n  reg u64 in, reg u64 inlen,\n  reg u64 kptr,\n  reg u64 out, reg u64 outlen\n) /*-> reg u64*/ {\n  while (in != end) {\n    m = [in + 0 * 8];\n    v[3] ^= m;\n    for i = 0 to cROUNDS {\n      v = sipround(v);\n    }\n    v[0] ^= m;\n    in += 8;\n  }\n\n  for i = 0 to cROUNDS {\n    v = sipround(v);\n  }\n}\n" in
  let res = parse_string code in
  let pos = params.position in
  match res with
  | Ok ast -> 
  let _node = node_of_pos pos ast in
    (* Logs.debug (fun m -> m "%d" node); *)
    []
  | Error _ -> []