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