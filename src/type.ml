open Misc

type tyvar = int
type typid = int
type constr_id = int
    
type typ =
  | TyInt
  | TyBool
  | TyFun  of typ * typ
  | TyList of typ
  | TyVar  of tyvar
  | TyName of typ list * typid

type typdef =
  | TdVariance of tyvar list * (constr_id * typ list) list
  | TdAlias of typ

type context =
    {
      typ_env: (Syntax.id * typ) Env.t;
      typvar_map: TypvarMap.t;
      typdef_env: (typid * typdef) Env.t;
    }

let rec map_typ typvar_map typdef_env = function
  | Syntax.FunT (arg, ret)   -> begin
      let (map, arg) = map_typ typvar_map typdef_env arg in
      let (map, ret) = map_typ typvar_map typdef_env ret in
      (typvar_map, TyFun (arg, ret))
    end
  | Syntax.VarT id            -> begin
      let typvar_map = TypvarMap.add id typvar_map in
      (typvar_map, TyVar (TypvarMap.find id typvar_map))
    end
  | Syntax.NameT (typs, name) -> begin
      let typvar_map, typs =
        List.fold_right
          (fun typ (typvar_map, typs) ->
             let typvar_map, typ = map_typ typvar_map typdef_env typ in
             (typvar_map, typ::typs))
          typs (typvar_map, [])
      in
      (typvar_map, TyName (typs, name))
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
  | (TyInt|TyFun _|TyBool|TyList _|TyVar _), _ -> false
      
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
  in
  (fun typ -> pps_typ_inner typ)

let rec pp_typ typ =
  print_string @< pps_typ typ
