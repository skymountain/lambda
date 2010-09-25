exception Matching_error of string

val ematch : Value.t -> Syntax.pat -> (string * Value.t) Env.t option
val tmatch :
  TypeContext.t -> Subst.t -> Type.typ -> Syntax.pat ->
  ((string * TypeScheme.t) Env.t) * Subst.t
