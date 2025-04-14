open Jsonrpc
open Lsp

let handle_request (type r) (server : Rpc_server.t) (request : r Client_request.t) (req_id : Id.t) = 
  match request with 
  | Client_request.Initialize (params : Types.InitializeParams.t) -> (
    Logs.debug (fun m -> m "init req");
    let server, init = Initialize_request.on_request server params in 
    let resp = LSP_.respond req_id request init in
    let _st = Packet.yojson_of_t resp |> Yojson.Safe.pretty_to_string in 
    server, resp
  )
  | _ -> (
    Logs.debug (fun m -> m "unknown req");
    server, LSP_.packet_of_request @@ Client_request.to_jsonrpc_request request ~id:(`Int 0)
  )

let on_request (server : Rpc_server.t) (req : Request.t) = 
  match (Client_request.of_jsonrpc req) with 
  | Ok (Client_request.E r) -> handle_request server r req.id
  | Error _ -> server, LSP_.packet_of_request req
