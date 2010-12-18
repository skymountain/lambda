type t

val add      : string -> unit
val mem      : string -> bool
val find     : string -> Type.typvar option
val refresh  : unit   -> unit
val cardinal : unit   -> int
