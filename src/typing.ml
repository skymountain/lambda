open Misc
open Syntax
open Type

exception Typing_error of string
let err s = raise (Typing_error (Printf.sprintf "Typing error: %s" s))
  
(* typing for constant *)
let typ_const typvar_map = function
    CInt _ -> (typvar_map, TyInt)
  | CBool _ -> (typvar_map, TyBool)
  | CNullList typ -> begin
      match typ with
        ListT _ -> map_typ typvar_map typ
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
let rec typ_exp ctx = function
    Var var -> begin
      match Env.lookup ctx.typ_env var with
        Some t -> (ctx.typvar_map, t)
      | None   -> err @< Printf.sprintf "%s is not bound" var
    end

  | Const c ->
      typ_const ctx.typvar_map c

  | BinOp (op, exp1, exp2) -> begin
      let typvar_map, typ1 = typ_exp ctx exp1 in
      let ctx = { ctx with typvar_map = typvar_map } in
      let typvar_map, typ2 = typ_exp ctx exp2 in
      (typvar_map, typ_binop typ1 typ2 op)
    end

  | IfExp (cond, then_exp, else_exp) -> begin
      match typ_exp ctx cond with
        (typvar_map, TyBool) -> begin
          let typvar_map, then_typ = typ_exp { ctx with typvar_map = typvar_map } then_exp in
          let typvar_map, else_typ = typ_exp { ctx with typvar_map = typvar_map } else_exp in
          if eq_typ then_typ else_typ then (typvar_map, then_typ)
          else err "types of then and else expressions must be same"
        end
      | _ -> err "type of conditional expression must be boolean"
    end

  | Fun (var, typ, body) ->
      let typvar_map, typ = map_typ ctx.typvar_map typ in
      let ctx = { ctx with typ_env = Env.extend ctx.typ_env var typ; typvar_map = typvar_map } in
      let typvar_map, btyp = typ_exp ctx body in
      (typvar_map, TyFun (typ, btyp))

  | App (exp1, exp2) -> begin
      match typ_exp ctx exp1 with
        (typvar_map, TyFun (arg_typ, ret_typ)) ->
          let typvar_map, arg_typ' = typ_exp { ctx with typvar_map = typvar_map } exp2 in
          if eq_typ arg_typ arg_typ' then (typvar_map, ret_typ)
          else err "type of actual argument must correspond with one of formal argument"
      | _ -> err "only function type can be applied"
    end

  | Let (var, exp, body) ->
      let typvar_map, typ = typ_exp ctx exp in
      let ctx = { ctx with typ_env = Env.extend ctx.typ_env var typ; typvar_map = typvar_map } in
      typ_exp ctx body

  | LetRec (var, typ, exp, body) ->
      let typvar_map, typ = typ_letrec ctx var typ exp in
      let ctx = { ctx with typ_env = Env.extend ctx.typ_env var typ; typvar_map = typvar_map } in
      typ_exp ctx body

  | ListLit exps -> begin
      let typvar_map, typs = List.fold_right
        (fun exp (typvar_map, typs) ->
           let typvar_map, typ = typ_exp { ctx with typvar_map = typvar_map } exp in
           (typvar_map, typ::typs))
        exps (ctx.typvar_map, [])
      in
      let typ, typs = match typs with typ::typs -> (typ, typs) | _ -> assert false in (* assume exps is not empty *)
      List.iter (fun typ' -> if not (eq_typ typ typ') then err "element types of list must be same") typs;
      (typvar_map, TyList typ)
    end

  | TypedExpr (exp, typ) -> begin
      let typvar_map, typ' = typ_exp ctx exp in
      let typvar_map, typ = map_typ typvar_map typ in
      if eq_typ typ typ' then (typvar_map, typ)
      else err "expression's type doesn't cossrespond with the specified type"
    end

  | MatchExp (exp, branches) -> begin
      let typvar_map, typ = typ_exp ctx exp in
      let rec iter typvar_map cond_typ = function
          [(pat, body)] -> begin
            let tenv' = Patmatch.tmatch err typvar_map cond_typ pat in
            let tenv = Env.extend_by_env ctx.typ_env tenv' in
            typ_exp { ctx with typ_env = tenv; typvar_map = typvar_map } body
          end
        | (pat, body)::t -> begin
            let tenv' = Patmatch.tmatch err typvar_map cond_typ pat in
            let tenv = Env.extend_by_env ctx.typ_env tenv' in
            let typvar_map ,btyp = typ_exp { ctx with typ_env = tenv; typvar_map = typvar_map } body in
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
and typ_letrec ctx var typ exp =
  let typvar_map, typ = map_typ ctx.typvar_map typ in
  match exp, typ with
    Fun _, TyFun _ -> begin
      let ctx = { ctx with typ_env = Env.extend ctx.typ_env var typ; typvar_map = typvar_map } in
      let typvar_map, etyp = typ_exp ctx exp in
      if eq_typ typ etyp then (typvar_map, etyp)
      else err "expression's type doesn't cossrespond with the specified type"
    end
  | _ -> err "only values which are functions can be defined recursively"

(* typing for program *)
let typing ctx =
  let return tenv var (typvar_map, typ) =
    { ctx with
      typ_env    = Env.extend tenv var typ;
      typvar_map = TypvarMap.refresh typvar_map;
    },
    var, typ;
  in
  function
    Exp exp -> return ctx.typ_env "it" @< typ_exp ctx exp
  | Decl (var, exp) -> return ctx.typ_env var @< typ_exp ctx exp
  | DeclRec (var, typ, exp) -> return ctx.typ_env var @< typ_letrec ctx var typ exp
