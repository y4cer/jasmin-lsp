open Jsonrpc
open Lsp

let log_msg msg =
  let oc = open_out_gen [Open_append] 0o666 "/tmp/logs.log" in
  Printf.fprintf oc "%s\n" msg;
  close_out oc;;

let handle_request (type r) (request : r Client_request.t) (req_id : Id.t) = 
  (* log_msg (Yojson.Safe.pretty_to_string @@ Request.yojson_of_t @@ Client_request.to_jsonrpc_request request ~id:(`Int 0) ); *)
  match request with 
  | Client_request.Initialize (_params : Types.InitializeParams.t) -> (
    let init = Initialize_request.on_request _params in 
    let resp = Initialize_request.respond req_id request init in
    let st = Packet.yojson_of_t resp |> Yojson.Safe.pretty_to_string in 
    log_msg st
  )
  | _ -> ()

let on_request (req : Request.t) = 
  match (Client_request.of_jsonrpc req) with 
  | Ok (Client_request.E r) -> handle_request r req.id; "req ok"
  | Error _ -> (); "req error"
