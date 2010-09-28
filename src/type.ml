open Misc
open Syntax

(* equality function *)
let rec eq_typ typ1 typ2 =
  match typ1, typ2 with
    IntT, IntT -> true
  | BoolT, BoolT -> true
  | FunT (arg1, ret1), FunT (arg2, ret2) ->
      eq_typ arg1 arg2 && eq_typ ret1 ret2
  | ListT typ1, ListT typ2 -> eq_typ typ1 typ2
  | (IntT|FunT _|BoolT|ListT _), _ -> false
      
(* pretty printer *)
let pps_typ =
  let is_funtyp_without_paren s =
    let len = String.length s in
    let rec iter idx depth =
      if idx >= len then false
      else match s.[idx] with
        '(' -> iter (idx+1) (depth+1)
      | ')' -> iter (idx+1) (depth-1)
      | '-' when idx+1 < len && s.[idx+1] = '>'-> true
      | _   -> iter (idx+1) depth
    in
    iter 0 0
  in
  let rec pps_typ_inner = function
      IntT  -> "int"
    | BoolT -> "bool"
    | FunT (typ1, typ2) -> begin
        let t1 = pps_typ_inner typ1 in
        let t1 = if is_funtyp_without_paren t1 then "("^t1^")" else t1 in
        let t2 = pps_typ_inner typ2 in
        t1^" -> "^t2
      end
    | ListT typ -> begin
        let t = pps_typ_inner typ in
        let t = if is_funtyp_without_paren t then "("^t^")" else t in
        t^" list"
      end
  in
  (fun typ -> pps_typ_inner typ)

let rec pp_typ typ =
  print_string @< pps_typ typ
    
