val typing :
  TypeContext.t -> Syntax.eval ->
  TypeContext.t * string * TypeScheme.t
val define_typ : TypeContext.t -> Syntax.typdef -> TypeContext.t
