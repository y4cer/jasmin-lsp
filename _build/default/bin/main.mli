val log_msg : string -> unit
val serialize_request_example : string -> string
module RPC_IO :
  sig
    val read : Lwt_io.input_channel -> Jsonrpc.Packet.t option Lwt.t
    val write : Lwt_io.output_channel -> Jsonrpc.Packet.t -> unit Lwt.t
  end
val handle_message : Jsonrpc.Packet.t -> unit
val read : unit -> Jsonrpc.Packet.t option Lwt.t
