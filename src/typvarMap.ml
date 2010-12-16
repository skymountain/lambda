module M = Map.Make(String)

type t = int M.t

let empty = M.empty
  
let add map s =
  if M.mem s map then map
  else M.add s (Types.newtypvar ()) map

let mem map k = M.mem k map
let find map k = try Some (M.find k map) with Not_found -> None
let compare = (M.compare compare : t -> t -> int)
let equal tvmap1 tvmap2 = compare tvmap1 tvmap2 = 0
