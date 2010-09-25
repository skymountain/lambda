open Misc

type typvar = int

and typ =
  | TyFun     of typ * typ
  | TyVar     of typvar
  | TyVariant of typ list * Ident.t
  | TyAlias   of typ * typ list * Ident.t

type typdef = {
  td_params : typvar list;
  td_arity  : int;
  td_kind   : typ_kind;
  td_id     : Ident.t;
}
and typ_kind =
  | TkVariant of (string * typ list) list
  | TkAlias of typ

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

let fresh_typvar () = TyVar (newtypvar ())

let fresh_typvar_list n =
  List.map (fun x -> TyVar x) @< newtypvar_list n
