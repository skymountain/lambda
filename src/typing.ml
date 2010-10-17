open Misc
open Syntax
open Common
open OptionMonad

let pps_typ = Printtyp.pps_typ

exception Typing_error of string
let err s = raise (Typing_error (Printf.sprintf "Typing error: %s" s))

(* unification utils *)
let unify_err s = err s

let unify_with_tenv subst tenv typ1 typ2 msg =
  match Subst.unify subst typ1 typ2 with
    None       -> unify_err msg
  | Some subst -> (Subst.subst_tenv subst tenv, subst)

let unifyl_with_tenv subst tenv typs msg =
  match Subst.unifyl subst typs with
    None       -> unify_err msg
  | Some subst -> (Subst.subst_tenv subst tenv, subst)

let return typ (tenv, subst) = (tenv, subst, Subst.subst_typ subst typ)
      
(* typing for unary operator *)
let typ_unaryop tenv subst typ = function
    Deref -> begin
      let ctyp = Type.fresh_typvar () in
      return ctyp @< unify_with_tenv subst tenv typ (RefT ctyp)
        @< Printf.sprintf "argument of %s must be reference type" @< str_of_unaryop Deref
    end
      
(* typing for binary operator *)
let typ_binop tenv subst typ1 typ2 = function
    (Plus | Minus | Mult | Div) as op ->
      return IntT @< unifyl_with_tenv subst tenv [(typ1, IntT); (typ2, IntT)]
        @< Printf.sprintf "both arguments of %s must be integer" @< str_of_binop op
  | Lt ->
      return BoolT @< unifyl_with_tenv subst tenv [(typ1, IntT); (typ2, IntT);]
        @< Printf.sprintf "both arguments of %s must be integer" @< str_of_binop Lt
  | Cons ->
      return typ2 @<
        unify_with_tenv subst tenv typ2 (ListT typ1)
        @< Printf.sprintf "right-side of %s must be type %s"
        (str_of_binop Cons) (pps_typ (ListT typ1))
  | Assign ->
      return typ2 @<
        unify_with_tenv subst tenv typ1 (RefT typ2)
        @< Printf.sprintf "left-side type of %s is %s, but was expected of type typ %s"
        (str_of_binop Assign) (pps_typ typ1) (pps_typ @< RefT typ2)
      
(* typing for exp *)
let rec typ_exp tenv subst = function
    Var var -> begin
      match Env.lookup tenv var with
        Some t -> (tenv, subst, Subst.subst_typ subst @< TypeScheme.instantiate t)
      | None   -> err @< Printf.sprintf "%s is not bound" var
    end

  | Const c -> (tenv, subst, Type.of_const c)

  | UnaryOp (op, exp) -> begin
      let tenv, subst, typ = typ_exp tenv subst exp in
      typ_unaryop tenv subst typ op
    end
      
  | BinOp (op, exp1, exp2) -> begin
      let tenv, subst, typ1 = typ_exp tenv subst exp1 in
      let tenv, subst, typ2 = typ_exp tenv subst exp2 in
      typ_binop tenv subst typ1 typ2 op
    end
      
  | IfExp (cond, then_exp, else_exp) -> begin
      let tenv, subst, ctyp = typ_exp tenv subst cond in
      let tenv, subst = unify_with_tenv subst tenv ctyp BoolT "type of conditional expression must be boolean" in
      let tenv, subst, then_typ = typ_exp tenv subst then_exp in
      let tenv, subst, else_typ = typ_exp tenv subst else_exp in
      let tenv, subst = unify_with_tenv subst tenv then_typ else_typ "types of then and else expressions must be same" in
      (tenv, subst, Subst.subst_typ subst then_typ)
    end
      
  | Fun (var, typ, body) -> begin
      let tenv, subst, rtyp = typ_exp (Env.extend tenv var @< TypeScheme.monotyp typ) subst body in
      let tenv = Env.remove tenv var in
      (tenv, subst, FunT (Subst.subst_typ subst typ, rtyp))
    end
      
  | App (exp1, exp2) -> begin
      let tenv, subst, funtyp = typ_exp tenv subst exp1 in
      let ftyp, rtyp = Type.fresh_typvar (), Type.fresh_typvar () in
      let funtyp' = FunT (ftyp, rtyp) in
      let tenv, subst = unify_with_tenv subst tenv funtyp funtyp' "only function type can be applied" in
      let tenv, subst, atyp = typ_exp tenv subst exp2 in
      let ftyp = Subst.subst_typ subst ftyp in
      let tenv, subst = unify_with_tenv subst tenv ftyp atyp
        @< Printf.sprintf "type of actual argument %s must correspond with one of formal argument %s"
        (pps_typ atyp) (pps_typ @< Subst.subst_typ subst rtyp)
      in
      (tenv, subst, Subst.subst_typ subst rtyp)
    end
      
  | Let (var, exp, body) -> begin
      let tenv, subst, typ = typ_exp tenv subst exp in
      let tenv, subst, typ = typ_exp (Env.extend tenv var @< TypeScheme.closure typ tenv exp) subst body in
      (Env.remove tenv var, subst, typ)
    end
      
  | LetRec (var, typ, exp, body) -> begin
      let tenv, subst, typ = typ_letrec tenv subst var typ exp in
      let tenv, subst, typ = typ_exp (Env.extend tenv var @< TypeScheme.closure typ tenv exp) subst body in
      (Env.remove tenv var, subst, typ)
    end
      
  | TypedExpr (exp, typ) -> begin
      let (tenv, subst, typ') = typ_exp tenv subst exp in
      let tenv, subst =
        unify_with_tenv subst tenv typ typ' "expression's type doesn't cossrespond with the specified type"
      in
      (tenv, subst, Subst.subst_typ subst typ)
    end
      
  | MatchExp (exp, branches) -> begin
      let msg ctyp ptyp = Printf.sprintf "type of this pattern is %s, but is expected of type %s"
                            (pps_typ ptyp) (pps_typ ctyp)
      in
      let tenv, subst, typ = typ_exp tenv subst exp in
      let rec iter tenv subst ctyp = function
          [(pat, body)] -> begin
            let btenv, subst, ptyp = Patmatch.tmatch err tenv subst pat in
            let btenv, subst = unify_with_tenv subst btenv ctyp ptyp @< msg ctyp ptyp in
            let (_, subst, btyp) = typ_exp btenv subst body in
            (Subst.subst_tenv subst tenv, subst, btyp)
          end
        | (pat, body)::t -> begin
            let btenv, subst, ptyp = Patmatch.tmatch err tenv subst pat in
            let btenv, subst = unify_with_tenv subst btenv ctyp ptyp @< msg ctyp ptyp in
            let (_, subst, btyp) = typ_exp btenv subst body in
            let tenv, subst, btyp' = iter (Subst.subst_tenv subst tenv) subst (Subst.subst_typ subst ctyp) t in
            let btyp = Subst.subst_typ subst btyp in
            match Subst.unify subst btyp btyp' with
              Some subst -> (Subst.subst_tenv subst tenv, subst, Subst.subst_typ subst btyp)
            | None       -> err @< Printf.sprintf "%s doesn't match with %s: all branch expresions must be same types"
                                     (pps_typ btyp) (pps_typ btyp')

          end
        | _ -> assert false
      in
      iter tenv subst typ branches
    end

  | RefExp exp -> begin
      let tenv, subst, typ = typ_exp tenv subst exp in
      (tenv, subst, RefT typ)
    end
      
(* typing for let-rec *)
and typ_letrec tenv subst var funtyp exp =
  let ftyp, rtyp = Type.fresh_typvar (), Type.fresh_typvar () in
  let funtyp' = FunT (ftyp, rtyp) in
  match exp with
    Fun _ -> begin
      let tenv, subst = unify_with_tenv subst tenv funtyp funtyp' "only values which are functions can be defined recursively" in
      let funtyp = Subst.subst_typ subst funtyp in
      let tenv = Env.extend tenv var @< TypeScheme.monotyp funtyp in
      let tenv, subst, etyp = typ_exp tenv subst exp in
      let tenv, subst = unify_with_tenv subst tenv funtyp etyp "expression's type doesn't cossrespond with a function type" in
      (Env.remove tenv var, subst, Subst.subst_typ subst etyp)
    end
  | _    -> err "only values which are functions can be defined recursively"
    
(* typing for program *)
let typing tenv =
  
  let return var exp (tenv, _, typ) =
    let typ = TypeScheme.closure typ tenv exp in
    Env.extend tenv var typ, var, typ
  in
  
  function
    Exp exp -> return "it" exp @< typ_exp tenv Subst.empty exp
  | Decl (var, exp) -> return var exp @< typ_exp tenv Subst.empty exp
  | DeclRec (var, typ, exp) -> return var exp @< typ_letrec tenv Subst.empty var typ exp
  | EOF -> assert false
