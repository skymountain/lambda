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
let pps_typvar id typvar_map typvar =
  try (typvar_map, typvar, TypvarMap.find id typvar_map) with
    Not_found -> (TypvarMap.add id typvar typvar_map, new_typvar typvar, typvar)

let map_with_paren = List.map
  (fun (with_paren, s) -> if with_paren then Printf.sprintf "(%s)" s else s)

(* replace %s with exact string and %p with string with parenthesis *)
let pps_template template fs tvmap typvar is_bound with_paren =
  let tvmap, typvar, xs =
    List.fold_left (fun (tvmap, typvar, xs) f ->
                      let tvmap, typvar, with_paren, s = f tvmap typvar is_bound in
                      (tvmap, typvar, (with_paren, s)::xs))
      (tvmap, typvar, []) fs
  in
  let xs = List.rev xs in
  let pre = ref 0 in
  let ret = ref "" in
  List.iter (fun (with_paren, to_str) ->
               let pos = String.index_from template !pre '%' in
               assert (pos + 1 < String.length template);
               let to_str =
                 if not with_paren then to_str
                 else match String.get template @< pos + 1 with
                 | 's' -> to_str
                 | 'p' -> Printf.sprintf "(%s)" to_str
                 | _   -> assert false
               in
               ret := !ret ^ (String.sub template !pre (pos - !pre)) ^ to_str;
               pre := pos + 2)
    xs;
  let len = String.length template - !pre in
  if len > 0 then 
    ret := !ret ^ (String.sub template !pre len);
  (tvmap, typvar, with_paren, !ret)

let pps =
  let rec pps_inner typ tvmap typvar is_bound = match typ with
    | TyFun (typ1, typ2) -> begin
        pps_template "%p -> %s" [(pps_inner typ1); (pps_inner typ2)] tvmap typvar is_bound true
      end
    | TyVar tv -> begin
        let tvmap, typvar, s = pps_typvar tv tvmap typvar in
        let s = "'" ^ (if is_bound tv then "" else "_") ^ s in
        (tvmap, typvar, false, s)
      end
    | TyVariant (typs, ident) | TyAlias (_, typs, ident) -> begin
        let template = make_list (List.length typs) "%p " +> String.concat "" +> (fun s -> s^"%s") in
        let fs = List.fold_right (fun typ acc -> (pps_inner typ)::acc)
          typs [(fun tvmap typvar _ -> (tvmap, typvar, false, Ident.name ident))]
        in
        pps_template template fs tvmap typvar is_bound false
      end
  in
  (fun is_bound typ -> let _, _, _, s = pps_inner typ TypvarMap.empty "a" is_bound in s)

let pps_typ = pps (fun _ -> true)
let pp_typ typ = print_string @< pps_typ typ
