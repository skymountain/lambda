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

(* pretty printer for type *)
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
  let module IntMap = Map.Make(struct
                                 type t = int
                                 let compare = compare
                               end)
  in
  let pps_typvar typvar_map next_typvar id =
    try (typvar_map, next_typvar, IntMap.find id typvar_map) with
      Not_found -> (IntMap.add id next_typvar typvar_map, new_typvar next_typvar, next_typvar)
  in
  let rec pps_typ_inner typvar_map next_typvar = function
      IntT  -> (typvar_map, next_typvar, "int")
    | BoolT -> (typvar_map, next_typvar, "bool")
    | FunT (typ1, typ2) -> begin
        let typvar_map, next_typvar, t1 = pps_typ_inner typvar_map next_typvar typ1 in
        let t1 = if is_funtyp_without_paren t1 then "("^t1^")" else t1 in
        let typvar_map, next_typvar, t2 = pps_typ_inner typvar_map next_typvar typ2 in
        (typvar_map, next_typvar, t1^" -> "^t2)
      end
    | TypVar (_ as id) -> begin
        let new_typvar_map, next_typvar, s = pps_typvar typvar_map next_typvar id in
        (new_typvar_map, next_typvar, "'"^s)
          end
  in
  (fun typ -> let _, _, s = pps_typ_inner IntMap.empty "a" typ in s)

let rec pp_typ typ =
  print_string @< pps_typ typ
