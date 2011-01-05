type t

module TypvarSet : Set.S with type elt = t
module TypvarMap : Map.S with type key = t

val newtypvar      : unit -> t
val newtypvar_list : int -> t list
val init_typvarmap : t list -> 'a list -> 'a TypvarMap.t
