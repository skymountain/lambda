exception Eval_error of string

val eval :
  (string * Value.t) Env.t -> Syntax.eval ->
  (string * Value.t) Env.t * string * Value.t
