type typvar = int
type constr_id = int

and typ =
  | TyFun     of typ * typ
  | TyVar     of typvar
  | TyVariant of typ list * Ident.t
  | TyAlias   of typ * typ list * Ident.t

let typvar_id = ref 0
let newtypvar () =
  typvar_id := !typvar_id + 1;
  !typvar_id
    
let rec newtypvar_list = function
    0 -> []
  | n when n > 0 -> (newtypvar ())::(newtypvar_list (n-1))
  | _ -> assert false

let rec replace_tyvar assoc = function
  | TyFun (atyp, rtyp)          -> TyFun (replace_tyvar assoc atyp, replace_tyvar assoc rtyp)
  | TyVar tv                    -> List.assoc tv assoc
  | TyVariant (typs, typdef)    -> TyVariant (List.map (replace_tyvar assoc) typs, typdef)
  | TyAlias (typ, typs, typdef) -> TyAlias (replace_tyvar assoc typ, List.map (replace_tyvar assoc) typs, typdef)

