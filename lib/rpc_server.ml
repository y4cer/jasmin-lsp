module State = struct
  type t = Uninitialized | Running | Stopped
end

type t = { custom : string; state : State.t }

let create msg =
  { custom = msg; state = State.Uninitialized }