open Jsonrpc
open Lsp
open Types

module SN = Server_notification

module State = struct
  type t = Uninitialized | Running | Stopped
end

(* See mli for a description *)
module Reply = struct
  type send = Packet.t -> unit Lwt.t
  type t = Now of Packet.t | Later of (send -> unit Lwt.t)

  let now r = Now r
  let later (f : send -> unit Lwt.t) = Later f
  let empty = Later (fun _ -> Lwt.return_unit)

  let apply (f : send) (x : t) =
    match x with
    | Now r -> f r
    | Later f' -> f' f

  let both (r1 : t) (r2 : t) =
    later (fun f ->
        let%lwt () = apply f r1 in
        apply f r2)
end

let packet_of_response (r : Response.t) = Packet.Response r
let packet_of_request (r : Request.t) = Packet.Request r
let packet_of_notification (n : Notification.t) = Packet.Notification n

let respond_json id result = packet_of_response (Response.ok id result)

let respond (type r) (id : Id.t) (request : r Client_request.t) (response : r) = 
  response |> Client_request.yojson_of_result request |> respond_json id

(** Send a notification to the client *)
let notify notification =
  let notification = SN.to_jsonrpc notification in
  Logs.debug (fun m ->
      m "Sending notification %s" notification.method_);
  packet_of_notification notification

let notify_show_message ~kind s =
  let notif =
    Server_notification.ShowMessage
      { ShowMessageParams.message = s; type_ = kind }
  in
  notify notif

let batch_notify notifications =
  Logs.debug (fun m -> m "Sending notifications");
  List.map notify notifications