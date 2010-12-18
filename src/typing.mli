val typing :
  TypeContext.t -> Syntax.eval ->
  TypeContext.t * string * Type.typ
val define_typ : TypeContext.t -> Syntax.typdef -> TypeContext.t
