open Rpc

let read () = RPC_IO.read Lwt_io.stdin

let write packet = RPC_IO.write Lwt_io.stdout packet

let flush () = Lwt_io.flush Lwt_io.stdout

let send packet = 
  let%lwt () = write packet in
  flush ()