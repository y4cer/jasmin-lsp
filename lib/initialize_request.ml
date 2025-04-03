open Lsp
open Types
open Jsonrpc

let on_request (_params : InitializeParams.t) =
  let capabilities = 
    ServerCapabilities.create
      ~textDocumentSync:
        (`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create ~openClose:true 
          ~change: TextDocumentSyncKind.Full ~save:(`Bool true) ())) ()
  in
  let init = 
    InitializeResult.
      { 
        capabilities = capabilities;
        serverInfo = Some { name = "jasmin-lsp"; version = Some "0.0.1" }
      }
  in init

let packet_of_response (r : Response.t) = Packet.Response r
let packet_of_request (r : Request.t) = Packet.Request r

let respond_json id result = packet_of_response (Response.ok id result)

let respond (type r) (id : Id.t) (request : r Client_request.t) (response : r) = 
  response |> Client_request.yojson_of_result request |> respond_json id