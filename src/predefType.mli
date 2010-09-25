val int_typ       : Type.typ
val bool_typ      : Type.typ
val inst_list_typ : Type.typ -> Type.typ
val etyp_of_list  : Type.typ -> Type.typ option
val new_listyp    : unit -> Type.typ * Type.typ
val predef_env    : (Ident.t * Type.typdef) Env.t
