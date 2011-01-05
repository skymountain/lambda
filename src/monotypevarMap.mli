type t

val add      : string -> unit
val mem      : string -> bool
val find     : string -> Typevar.t option
val refresh  : unit   -> unit
val cardinal : unit   -> int
