open Misc
open Typevar
open Type
open Common

let new_typvar typvar =
  assert (String.length typvar > 0);
  let len = String.length typvar in
  let rec iter s idx =
    match idx, s.[idx] with
      0, 'z' -> begin
        let len = String.length s in
        let s' = String.create @< len + 1 in
        String.blit s 0 s' 0 len;
        s'.[0] <- 'a';
        s'.[len] <- 'a';
        s'
      end
    | _, 'z' -> begin
        s.[idx] <- 'a';
        iter s @< idx - 1
      end
    | _, _ -> begin
        s.[idx] <- char_of_int @< (int_of_char s.[idx] + 1);
        s
      end
  in
  iter (String.copy typvar) (len - 1)

(* pretty printer for types *)
let pps =
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
  let pps_typvar typvar_map typvar id =
    try (typvar_map, typvar, TypvarMap.find id typvar_map) with
      Not_found -> (TypvarMap.add id typvar typvar_map, new_typvar typvar, typvar)
  in
  let rec pps_inner tvmap typvar is_bound = function
    | TyFun (typ1, typ2) -> begin
        let tvmap, typvar, t1 = pps_inner tvmap typvar is_bound typ1 in
        let t1 = if is_funtyp_without_paren t1 then "("^t1^")" else t1 in
        let tvmap, typvar, t2 = pps_inner tvmap typvar is_bound typ2 in
        (tvmap, typvar, t1^" -> "^t2)
      end
    | TyVar tv -> begin
        let tvmap, typvar, s = pps_typvar tvmap typvar tv in
        let s = "'" ^ (if is_bound tv then "" else "_") ^ s in
        (tvmap, typvar, s)
      end
    | TyVariant (typs, ident) | TyAlias (_, typs, ident) -> begin
        let tvmap, typvar, typ_strs = pps_typ_list_inner tvmap typvar is_bound typs in
        let params_str = String.concat " " typ_strs in
        let typ_name = Ident.name ident in
        if String.length params_str = 0 then (tvmap, typvar, typ_name)
        else (tvmap, typvar, Printf.sprintf "%s %s" params_str typ_name)
      end
  and pps_typ_list_inner tvmap typvar is_bound typs =
    let tvmap, typvar, typ_strs =
      List.fold_left (fun (tvmap, typvar, acc) typ ->
                        let tvmap, typvar, s = pps_inner tvmap typvar is_bound typ in
                        (tvmap, typvar, s::acc))
        (tvmap, typvar, []) typs
    in
    (tvmap, typvar, List.rev typ_strs)
  in
  (fun is_bound typ -> let _, _, s = pps_inner TypvarMap.empty "a" is_bound typ in s)


let pps_typ = pps (fun _ -> true)
let pp_typ typ = print_string @< pps_typ typ
