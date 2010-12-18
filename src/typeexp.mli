exception Typing_error of string

val err           : string -> 'a
val replace_tyvar : (Type.typvar * Type.typ) list -> Type.typ -> Type.typ
val map_typ       : TypeContext.t -> Syntax.typ -> Type.typ
val map_typs      : TypeContext.t -> Syntax.typ list -> Type.typ list
val eq_typ        : Type.typ -> Type.typ -> bool
