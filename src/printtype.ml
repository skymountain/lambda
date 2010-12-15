open Misc
open Types

(* pretty printer for types *)
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
    | TyFun (typ1, typ2) -> begin
        let t1 = pps_typ_inner typ1 in
        let t1 = if is_funtyp_without_paren t1 then "("^t1^")" else t1 in
        let t2 = pps_typ_inner typ2 in
        t1^" -> "^t2
      end
    | TyVar tv -> string_of_int tv
    | TyVariant (typs, ident) | TyAlias (_, typs, ident) -> begin
        let params_str = String.concat " " (List.map pps_typ_inner typs) in
        let typ_name = Ident.name ident in
        if String.length params_str = 0 then typ_name
        else Printf.sprintf "%s %s" params_str typ_name
      end
  in
  (fun typ -> pps_typ_inner typ)

let rec pp_typ typ =
  print_string @< pps_typ typ
