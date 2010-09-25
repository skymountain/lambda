type typvar

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

module TypvarSet : Set.S with type elt = typvar
module TypvarMap : Map.S with type key = typvar

val init_typvarmap : typvar list -> typ list -> typ TypvarMap.t

val newtypvar      : unit -> typvar
val newtypvar_list : int -> typvar list
val fresh_typvar      : unit -> typ
val fresh_typvar_list : int -> typ list
