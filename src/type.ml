open Misc
open Types

type context =
    {
      typ_env: (Syntax.id * typ) Env.t;
      typvar_map: TypvarMap.t;
      typdef_env: (string * typdef) Env.t;
    }

let rec replace_tyvar assoc = function
  | TyInt                       -> TyInt
  | TyBool                      -> TyBool
  | TyFun (atyp, rtyp)          -> TyFun (replace_tyvar assoc atyp, replace_tyvar assoc rtyp)
  | TyList typ                  -> TyList (replace_tyvar assoc typ)
  | TyVar tv                    -> List.assoc tv assoc
  | TyVariant (typs, typdef)    -> TyVariant (List.map (replace_tyvar assoc) typs, typdef)
  | TyAlias (typ, typs, typdef) -> TyAlias (replace_tyvar assoc typ, List.map (replace_tyvar assoc) typs, typdef)

let rec map_typ tctx = function
  | Syntax.FunT (arg, ret)    -> begin
      let (typvar_map, arg) = map_typ tctx arg in
      let (typvar_map, ret) = map_typ { tctx with typvar_map = typvar_map } ret in
      (typvar_map, TyFun (arg, ret))
    end
  | Syntax.VarT id            -> begin
      let typvar_map = TypvarMap.add id tctx.typvar_map in
      (typvar_map, TyVar (TypvarMap.find id typvar_map))
    end
  | Syntax.NameT (typs, name) -> begin
      match Env.lookup tctx.typdef_env name with
      | None ->
          assert false (* XXX *)
      | Some typdef when typdef.td_arity <> List.length typs ->
          assert false (* XXX *)
      | Some typdef -> begin
          let typvar_map, typs =
            List.fold_right
              (fun typ (typvar_map, typs) ->
                 let typvar_map, typ = map_typ { tctx with typvar_map = typvar_map } typ in
                 (typvar_map, typ::typs))
              typs (tctx.typvar_map, [])
          in
          match typdef.td_kind with
          | TkVariant _ -> (typvar_map, TyVariant (typs, typdef))
          | TkAlias typ -> (typvar_map, TyAlias (replace_tyvar (List.combine typdef.td_params typs) typ, typs, typdef))
        end
    end

(* equality function *)
let rec eq_typ typ1 typ2 =
  match typ1, typ2 with
    TyInt, TyInt -> true
  | TyBool, TyBool -> true
  | TyFun (arg1, ret1), TyFun (arg2, ret2) ->
      eq_typ arg1 arg2 && eq_typ ret1 ret2
  | TyList typ1, TyList typ2 -> eq_typ typ1 typ2
  | TyVar tv1, TyVar tv2 -> tv1 = tv2

  | TyVariant (typs1, typdef1), TyVariant (typs2, typdef2) -> begin
      typdef1.td_id = typdef2.td_id &&
        List.fold_left
          (fun acc (typ1, typ2) -> acc && eq_typ typ1 typ2)
            false (List.combine typs1 typs2)
    end
  | (TyAlias (atyp, _, _), typ) | (typ, TyAlias (atyp, _, _)) -> eq_typ atyp typ
  | (TyInt|TyFun _|TyBool|TyList _|TyVar _|TyVariant _), _ -> false

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
      TyInt  -> "int"
    | TyBool -> "bool"
    | TyFun (typ1, typ2) -> begin
        let t1 = pps_typ_inner typ1 in
        let t1 = if is_funtyp_without_paren t1 then "("^t1^")" else t1 in
        let t2 = pps_typ_inner typ2 in
        t1^" -> "^t2
      end
    | TyList typ -> begin
        let t = pps_typ_inner typ in
        let t = if is_funtyp_without_paren t then "("^t^")" else t in
        t^" list"
      end
    | TyVar tv -> string_of_int tv
    | TyVariant (typs, typdef) | TyAlias (_, typs, typdef) -> begin
        let params_str = String.concat " " (List.map pps_typ_inner typs) in
        Printf.sprintf "%s %s" params_str typdef.td_name
      end
  in
  (fun typ -> pps_typ_inner typ)

let rec pp_typ typ =
  print_string @< pps_typ typ
