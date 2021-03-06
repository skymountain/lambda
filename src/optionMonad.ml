let (>>=) x f =
  match x with
    Some x -> f x
  | None   -> None

let (>>) _ y = y

let return x = Some x

let fold_left f =
  List.fold_left (fun acc x -> acc >>= (fun acc -> f acc x)) 
