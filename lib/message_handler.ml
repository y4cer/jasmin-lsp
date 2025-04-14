open Jsonrpc
open Request_handler
open Notification_handler

let handle_message (server : Rpc_server.t) (msg : Packet.t) =
  Logs.debug (fun m -> m "handling message");
  match msg with 
  | Request req -> (
    Logs.debug (fun m -> m "req");
    on_request server req
  )
  | Notification notification -> (
    Logs.debug (fun m -> m "notification");
    let server, reply = handle_notification server notification in
    let res = 
      match reply with
        | Some packet -> packet
        | None -> msg
    in server, res
  )
  | _ -> (
    Logs.debug (fun m -> m "unknown"); 
    server, msg
  )