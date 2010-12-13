open Misc

type tyvar = int
type typ =
    TyInt
  | TyBool
  | TyFun  of typ * typ
  | TyList of typ
  | TyVar  of tyvar
      
let rec map_typ map = function
    Syntax.IntT            -> (map, TyInt)
  | Syntax.BoolT           -> (map, TyBool)
  | Syntax.FunT (arg, ret) -> begin
      let (map, arg) = map_typ map arg in
      let (map, ret) = map_typ map ret in
      (map, TyFun (arg, ret))
    end
  | Syntax.ListT typ       -> begin
      let (map, typ) = map_typ map typ in
      (map, TyList typ)
    end
  | Syntax.VarT id         -> begin
      let map = TypvarMap.add id map in
      (map, TyVar (TypvarMap.find id map))
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

