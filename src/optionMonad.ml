type 'a t = 'a option

let (>>=) x f =
  match x with
    Some x -> f x
  | None   -> None

let (>>) x y = y

let return x = Some x
