open Type

module M = Map.Make(String)

type t = typvar M.t

let map = ref (M.empty : typvar M.t)

let add s =
  if not (M.mem s !map) then map := M.add s (newtypvar ()) !map
let mem k =  M.mem k !map
let find k = try Some (M.find k !map) with Not_found -> None
let refresh () = map := M.empty
let cardinal () = M.cardinal !map