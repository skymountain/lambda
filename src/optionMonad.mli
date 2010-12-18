val (>>=)     : 'a option -> ('a -> 'b option) -> 'b option
val (>>)      : 'a -> 'b option -> 'b option
val return    : 'a -> 'a option
val fold_left : ('a -> 'b -> 'a option) -> 'a option -> 'b list -> 'a option
