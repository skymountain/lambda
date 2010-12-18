type t =
  | IntV  of int
  | BoolV of bool
  | FunV  of string * Syntax.exp * (string * t) Env.t ref
  | ListV of t list
  | ConstrV of string * t list

val pps_val : t -> string
val pp_val  : t -> unit
