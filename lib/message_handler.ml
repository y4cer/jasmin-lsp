open Jsonrpc
open Request_handler
open Notification_handler

let handle_message (msg : Packet.t) =
  Logs.debug (fun m -> m "handling message");
  match msg with 
  | Request req -> (
    Logs.debug (fun m -> m "req");
    on_request req
  )
  | Notification notification -> (
    Logs.debug (fun m -> m "notification");
    match handle_notification notification with
    | Some packet -> packet
    | None -> msg
  )
  | _ -> (
    Logs.debug (fun m -> m "unknown"); 
    msg
  )