open Jsonrpc
(* open Lsp *)

let log_msg msg =
  let oc = open_out_gen [Open_trunc; Open_append] 0o600 "/tmp/logs.log" in
  Printf.fprintf oc "%s\n" msg;
  close_out oc;;

let serialize_request_example id =
  (* Create a simple request to call a "subtract" method with two parameters *)

  let request = Jsonrpc.Request.create 
    ~id:(`String id)
    ~method_:"subtract" 
    () in
  
  (* Serialize the request to a JSON string *)
  let json_str = Jsonrpc.Request.yojson_of_t request |> Yojson.Safe.to_string in
  log_msg json_str;

  (json_str)

let _ = serialize_request_example

module RPC_IO = 
  Lsp.Io.Make
      (struct
        include Lwt

        module O = struct
          let ( let* ) x f = Lwt.bind x f
          let ( let+ ) x f = Lwt.map f x
        end

        let raise exn = Lwt.fail exn
      end)
      (struct
        type input = Lwt_io.input_channel
        type output = Lwt_io.output_channel

        let read_line = Lwt_io.read_line_opt

        let write output strings =
          Lwt_io.atomic
            (fun output ->
              Lwt_list.iter_s (fun str -> Lwt_io.write output str) strings)
            output

        (* LWT doesn't implement this in a nice way *)
        let read_exactly inc n =
          let rec read_exactly acc n =
            if n = 0 then
              let result = String.concat "" (List.rev acc) in
              Lwt.return (Some result)
            else
              let%lwt line = Lwt_io.read ~count:n inc in
              read_exactly (line :: acc) (n - String.length line)
          in
          read_exactly [] n
      end)

let handle_message (msg : Packet.t) =
  let a = Yojson.Safe.pretty_to_string @@ Packet.yojson_of_t msg in
  log_msg a;
  ()

let read () = RPC_IO.read Lwt_io.stdin

let () = 
  let _rpc_loop = Lwt_main.run (
    let%lwt client_msg = read () in
    match client_msg with
    | Some msg ->
      handle_message msg;
      Lwt.return_unit
    | None ->
      log_msg "aboba";
      Lwt.return_unit
  )
  in ()
