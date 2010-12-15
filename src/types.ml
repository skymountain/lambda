type tyvar = int
type typid = int
type constr_id = int

and typ =
  | TyInt
  | TyBool
  | TyFun     of typ * typ
  | TyList    of typ
  | TyVar     of tyvar
  | TyVariant of typ list * Ident.t
  | TyAlias   of typ * typ list * Ident.t

and typdef = {
  td_params : tyvar list;
  td_arity  : int;
  td_kind   : typ_kind;
  td_name   : string;
  td_id     : int;
}

and typ_kind =
  | TkVariant of (string * typ list) list
  | TkAlias of typ

let typvar_id = ref 0
let newtypvar () =
  typvar_id := !typvar_id + 1;
  !typvar_id
