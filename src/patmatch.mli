exception Matching_error of string

val ematch : Value.t -> Syntax.pat -> (string * Value.t) Env.t option
val tmatch :
  TypeContext.t -> Type.typ -> Syntax.pat ->
  (string * Type.typ) Env.t
