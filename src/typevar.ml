open Misc

type t = int
type typvar = t

module TypvarSet = Set.Make(struct
                              type t = typvar
                              let compare = compare
                            end)

module TypvarMap = Map.Make(struct
                              type t = typvar
                              let compare = compare
                            end)

let init_typvarmap typvars typs =
  List.fold_left
    (fun acc (typvar, typ) -> TypvarMap.add typvar typ acc)
    TypvarMap.empty @< List.combine typvars typs

let typvar_id = ref 0

let newtypvar () =
  typvar_id := !typvar_id + 1;
  !typvar_id

let rec newtypvar_list = function
    0 -> []
  | n when n > 0 -> (newtypvar ())::(newtypvar_list (n-1))
  | _ -> assert false
