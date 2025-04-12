open Jsonrpc
open Lsp

let handle_request (type r) (request : r Client_request.t) (req_id : Id.t) = 
  match request with 
  | Client_request.Initialize (_params : Types.InitializeParams.t) -> (
    Logs.debug (fun m -> m "init req");
    let init = Initialize_request.on_request _params in 
    let resp = LSP_.respond req_id request init in
    let _st = Packet.yojson_of_t resp |> Yojson.Safe.pretty_to_string in 
    resp
  )
  | _ -> (
    Logs.debug (fun m -> m "unknown req");
    LSP_.packet_of_request @@ Client_request.to_jsonrpc_request request ~id:(`Int 0)
  )

let on_request (req : Request.t) = 
  match (Client_request.of_jsonrpc req) with 
  | Ok (Client_request.E r) -> handle_request r req.id
  | Error _ -> LSP_.packet_of_request req
