type t = int * string

let counter = ref -1
let create s =
  counter := !counter + 1;
  (!counter, s)

let equal = (=)

let name (_, s) = s
