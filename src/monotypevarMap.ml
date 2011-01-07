open Typevar

module M = Map.Make(String)

type t = Typevar.t M.t

let map = ref (M.empty : Typevar.t M.t)

let add s =
  try M.find s !map with
  | Not_found -> begin
      let tv = newtypvar () in
      map := M.add s tv !map;
      tv
    end

let mem k =  M.mem k !map
let find k = try Some (M.find k !map) with Not_found -> None
let refresh () = map := M.empty
let cardinal () = M.cardinal !map
