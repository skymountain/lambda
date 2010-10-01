open Misc
open Syntax

(* type variable *)
let fresh_typvar =
  let id = ref 0 in
  let f () =
    let new_id = !id in
    id := !id + 1;
    TypVar new_id
  in
  f

let of_const = function
    CInt _ -> IntT
  | CBool _ -> BoolT
  | CNullList -> ListT (fresh_typvar ())
    
