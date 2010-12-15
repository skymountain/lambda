open Misc
open Syntax
open Types
open Type

exception Typing_error of string
let err s = raise (Typing_error (Printf.sprintf "Typing error: %s" s))
  
(* typing for constant *)
let typ_const tctx = function
    CInt _ -> (tctx.typvar_map, TyInt)
  | CBool _ -> (tctx.typvar_map, TyBool)
  | CNullList typ -> begin
      match typ with
        NameT _ -> map_typ tctx typ
      | _       -> err "specified type isn't list type"
    end

(* typing for binary operator *)
let typ_binop typ1 typ2 = function
    (Plus | Minus | Mult | Div) as op ->
      if typ1 = TyInt && typ2 = TyInt then TyInt
      else err @< Printf.sprintf "both arguments of %s must be integer" @< str_of_binop op
  | Lt ->
      if typ1 = TyInt && typ2 = TyInt then TyBool
      else err @< Printf.sprintf "both arguments of %s must be integer" @< str_of_binop Lt
  | Cons -> begin
      match typ2 with
        TyList etyp ->
          if eq_typ typ1 etyp then TyList etyp
          else err @< Printf.sprintf "element types of %s must be same types" @< str_of_binop Cons
      | _ -> err @< Printf.sprintf "right-side of %s must be list type" @< str_of_binop Cons
    end

(* typing for exp *)
let rec typ_exp tctx = function
    Var var -> begin
      match Env.lookup tctx.typ_env var with
        Some t -> (tctx.typvar_map, t)
      | None   -> err @< Printf.sprintf "%s is not bound" var
    end

  | Const c ->
      typ_const tctx c

  | BinOp (op, exp1, exp2) -> begin
      let typvar_map, typ1 = typ_exp tctx exp1 in
      let tctx = { tctx with typvar_map = typvar_map } in
      let typvar_map, typ2 = typ_exp tctx exp2 in
      (typvar_map, typ_binop typ1 typ2 op)
    end

  | IfExp (cond, then_exp, else_exp) -> begin
      match typ_exp tctx cond with
        (typvar_map, TyBool) -> begin
          let typvar_map, then_typ = typ_exp { tctx with typvar_map = typvar_map } then_exp in
          let typvar_map, else_typ = typ_exp { tctx with typvar_map = typvar_map } else_exp in
          if eq_typ then_typ else_typ then (typvar_map, then_typ)
          else err "types of then and else expressions must be same"
        end
      | _ -> err "type of conditional expression must be boolean"
    end

  | Fun (var, typ, body) ->
      let typvar_map, typ = map_typ tctx typ in
      let tctx = { tctx with typ_env = Env.extend tctx.typ_env var typ; typvar_map = typvar_map } in
      let typvar_map, btyp = typ_exp tctx body in
      (typvar_map, TyFun (typ, btyp))

  | App (exp1, exp2) -> begin
      match typ_exp tctx exp1 with
        (typvar_map, TyFun (arg_typ, ret_typ)) ->
          let typvar_map, arg_typ' = typ_exp { tctx with typvar_map = typvar_map } exp2 in
          if eq_typ arg_typ arg_typ' then (typvar_map, ret_typ)
          else err "type of actual argument must correspond with one of formal argument"
      | _ -> err "only function type can be applied"
    end

  | Let (var, exp, body) ->
      let typvar_map, typ = typ_exp tctx exp in
      let tctx = { tctx with typ_env = Env.extend tctx.typ_env var typ; typvar_map = typvar_map } in
      typ_exp tctx body

  | LetRec (var, typ, exp, body) ->
      let typvar_map, typ = typ_letrec tctx var typ exp in
      let tctx = { tctx with typ_env = Env.extend tctx.typ_env var typ; typvar_map = typvar_map } in
      typ_exp tctx body

  | ListLit exps -> begin
      let typvar_map, typs = List.fold_right
        (fun exp (typvar_map, typs) ->
           let typvar_map, typ = typ_exp { tctx with typvar_map = typvar_map } exp in
           (typvar_map, typ::typs))
        exps (tctx.typvar_map, [])
      in
      let typ, typs = match typs with typ::typs -> (typ, typs) | _ -> assert false in (* assume exps is not empty *)
      List.iter (fun typ' -> if not (eq_typ typ typ') then err "element types of list must be same") typs;
      (typvar_map, TyList typ)
    end

  | TypedExpr (exp, typ) -> begin
      let typvar_map, typ' = typ_exp tctx exp in
      let typvar_map, typ = map_typ { tctx with typvar_map = typvar_map } typ in
      if eq_typ typ typ' then (typvar_map, typ)
      else err "expression's type doesn't cossrespond with the specified type"
    end

  | MatchExp (exp, branches) -> begin
      let typvar_map, typ = typ_exp tctx exp in
      let rec iter typvar_map cond_typ = function
          [(pat, body)] -> begin
            let tctx = { tctx with typvar_map = typvar_map } in
            let tenv = Patmatch.tmatch err tctx cond_typ pat in
            let tctx = { tctx with typ_env = Env.extend_by_env tctx.typ_env tenv } in
            typ_exp tctx  body
          end
        | (pat, body)::t -> begin
            let tenv = Env.extend_by_env tctx.typ_env @< Patmatch.tmatch err tctx cond_typ pat in
            let typvar_map ,btyp = typ_exp { tctx with typ_env = tenv; typvar_map = typvar_map } body in
            let typvar_map, btyp' = iter typvar_map cond_typ t in
            if eq_typ btyp btyp' then (typvar_map, btyp)
            else err @< Printf.sprintf "%s doesn't match with %s: all branch expresions must be same types"
              (pps_typ btyp) (pps_typ btyp')
          end
        | _ -> assert false
      in
      iter typvar_map typ branches
    end

(* typing for let-rec *)
and typ_letrec tctx var typ exp =
  let typvar_map, typ = map_typ tctx typ in
  match exp, typ with
    Fun _, TyFun _ -> begin
      let typvar_map, etyp = typ_exp { tctx with typ_env = Env.extend tctx.typ_env var typ; typvar_map = typvar_map } exp in
      if eq_typ typ etyp then (typvar_map, etyp)
      else err "expression's type doesn't cossrespond with the specified type"
    end
  | _ -> err "only values which are functions can be defined recursively"

(* typing for program *)
let typing tctx =
  let return tenv var (typvar_map, typ) =
    { tctx with
      typ_env    = Env.extend tenv var typ;
      typvar_map = typvar_map;
    },
    var, typ;
  in
  function
    Exp exp -> return tctx.typ_env "it" @< typ_exp tctx exp
  | Decl (var, exp) -> return tctx.typ_env var @< typ_exp tctx exp
  | DeclRec (var, typ, exp) -> return tctx.typ_env var @< typ_letrec tctx var typ exp
