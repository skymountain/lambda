open Misc
open Syntax
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
  let module TypVarMap = Map.Make(struct
                                 type t = typvar
                                 let compare = compare
                               end)
  in
  let pps_typvar typvar_map typvar id =
    try (typvar_map, typvar, TypVarMap.find id typvar_map) with
      Not_found -> (TypVarMap.add id typvar typvar_map, new_typvar typvar, typvar)
  in
  let rec iter typvar_map typvar is_bound = function
      IntT  -> (typvar_map, typvar, "int")
    | BoolT -> (typvar_map, typvar, "bool")
    | FunT (typ1, typ2) -> begin
        let typvar_map, typvar, t1 = iter typvar_map typvar is_bound typ1 in
        let t1 = if is_funtyp_without_paren t1 then "("^t1^")" else t1 in
        let typvar_map, typvar, t2 = iter typvar_map typvar is_bound typ2 in
        (typvar_map, typvar, t1^" -> "^t2)
      end
    | TypVar id -> begin
        let typvar_map, typvar, s = pps_typvar typvar_map typvar id in
        let s = "'" ^ (if is_bound id then "" else "_") ^ s in
        (typvar_map, typvar, s)
      end
    | ListT typ -> begin
        let typvar_map, typvar, t = iter typvar_map typvar is_bound typ in
        let t = if is_funtyp_without_paren t then "("^t^")" else t in
        (typvar_map, typvar, t^" list")
      end
    | RefT typ -> begin
        let typvar_map, typvar, t = iter typvar_map typvar is_bound typ in
        let t = if is_funtyp_without_paren t then "("^t^")" else t in
        (typvar_map, typvar, t^" ref")
      end
  in
  (fun is_bound typ -> let _, _, s = iter TypVarMap.empty "a" is_bound typ in s)

let pps_typ = pps (fun _ -> true)
let pp_typ typ = print_string @< pps_typ typ

let pps_typscheme typ =
  let bvars, typ = TypeScheme.bound_typvars typ, TypeScheme.typ typ in
  pps (fun id -> TypVarSet.mem id bvars) typ
let pp_typscheme typ = print_string @< pps_typscheme typ
