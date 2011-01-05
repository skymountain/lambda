open Misc

type typ =
  | TyFun     of typ * typ
  | TyVar     of Typevar.t
  | TyVariant of typ list * Ident.t
  | TyAlias   of typ * typ list * Ident.t

type typdef = {
  td_params : Typevar.t list;
  td_arity  : int;
  td_kind   : typ_kind;
  td_id     : Ident.t;
}
and typ_kind =
  | TkVariant of (string * typ list) list
  | TkAlias of typ
