exception Typing_error of string

val err           : string -> 'a
val replace_tyvar : Type.typ Type.TypvarMap.t -> Type.typ -> Type.typ
val map_typ       : TypeContext.t -> Syntax.typ -> Type.typ
val map_typs      : TypeContext.t -> Syntax.typ list -> Type.typ list
val eq_typ        : Type.typ -> Type.typ -> bool
val variant       : Type.typ -> (Type.typ list * Ident.t) option
val variant_constr: TypeContext.t -> string -> (Type.typdef * Type.typ list) option
val local_unify   : Type.typ -> Type.typ -> Type.typ Type.TypvarMap.t option
