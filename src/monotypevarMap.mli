type t

val add      : string -> Typevar.t
val mem      : string -> bool
val find     : string -> Typevar.t option
val refresh  : unit   -> unit
val cardinal : unit   -> int
