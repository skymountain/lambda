open Misc
open Syntax
open Common
open OptionMonad;;

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

(* typing for binary operator *)
let typ_binop tenv subst typ1 typ2 =
  function
    (Plus | Minus | Mult | Div) as op -> begin
      let err_msg = Printf.sprintf "both arguments of %s must be integer" @< str_of_binop op in
      let tenv, subst = unifyl_with_tenv subst tenv [(typ1, IntT); (typ2, IntT)] err_msg in
      (tenv, subst, IntT)
    end
  | Lt -> begin
      let err_msg = Printf.sprintf "both arguments of %s must be integer" @< str_of_binop Lt in
      let tenv, subst = unifyl_with_tenv subst tenv [(typ1, IntT); (typ2, IntT);] err_msg in
      (tenv, subst, BoolT)
    end
  | Eq -> begin
      let err_msg = Printf.sprintf "both arguments of %s must be same types" @< str_of_binop Eq in
      let tenv, subst = unify_with_tenv subst tenv typ1 typ2 err_msg in
      (tenv, subst , BoolT)
    end
  | Cons -> begin
      let etyp = Type.fresh_typvar () in
      let ltyp = ListT etyp in
      let tenv, subst = unify_with_tenv subst tenv typ2 ltyp
        @< Printf.sprintf "right-side of %s must be list type" @< str_of_binop Cons in
      let tenv, subst = unify_with_tenv subst tenv typ1 etyp
        @< Printf.sprintf "element types of %s must be same types" @< str_of_binop Cons in
      (tenv, subst, ListT (Subst.subst_typ subst etyp))
    end
      
(* typing for exp *)
let rec typ_exp tenv subst = function
    Var var -> begin
      match Env.lookup tenv var with
        Some t -> (tenv, subst, Subst.subst_typ subst @< TypeScheme.instantiate t)
      | None   -> err @< Printf.sprintf "%s is not bound" var
    end

  | Const c -> (tenv, subst, Type.of_const c)
      
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
      let tenv, subst, rtyp = typ_exp (Env.extend tenv var @< TypeScheme.make TypVarSet.empty typ) subst body in
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
      let tenv, subst = unify_with_tenv subst tenv ftyp atyp "type of actual argument must correspond with one of formal argument" in
      (tenv, subst, Subst.subst_typ subst rtyp)
    end
      
  | Let (var, exp, body) -> begin
      let tenv, subst, typ = typ_exp tenv subst exp in
      let tenv, subst, typ = typ_exp (Env.extend tenv var @< TypeScheme.closure typ tenv) subst body in
      (Env.remove tenv var, subst, typ)
    end
      
  | LetRec (var, typ, exp, body) -> begin
      let tenv, subst, typ = typ_letrec tenv subst var typ exp in
      let tenv, subst, typ = typ_exp (Env.extend tenv var @< TypeScheme.closure typ tenv) subst body in
      (Env.remove tenv var, subst, typ)
    end

  | ListLit exps -> begin
      let etyp = Type.fresh_typvar () in
      let tenv, subst = List.fold_left (fun (tenv, subst) exp ->
                                          let tenv, subst, typ = typ_exp tenv subst exp in
                                          unify_with_tenv subst tenv typ etyp "element types of list must be same")
        (tenv, subst) exps
      in
      (tenv, subst, ListT (Subst.subst_typ subst etyp))
    end
      
  | TypedExpr (exp, typ) -> begin
      let (tenv, subst, typ') = typ_exp tenv subst exp in
      let tenv, subst =
        unify_with_tenv subst tenv typ typ' "expression's type doesn't cossrespond with the specified type"
      in
      (tenv, subst, Subst.subst_typ subst typ)
    end
      
  | MatchExp (exp, branches) -> begin
      let tenv, subst, typ = typ_exp tenv subst exp in
      let typscheme_env_of tenv = Env.map tenv (fun typ -> TypeScheme.make (TypVarSet.empty) typ) in
      let rec iter subst cond_typ = function
          [(pat, body)] -> begin
            let btenv, subst = Patmatch.tmatch err subst cond_typ pat in
            let btenv = typscheme_env_of btenv in
            let btenv = Env.extend_by_env tenv btenv in
            let (_, subst, btyp) = typ_exp btenv subst body in
            (subst, btyp)
          end
        | (pat, body)::t -> begin
            let (btenv, subst) = Patmatch.tmatch err subst cond_typ pat in
            let btenv =  typscheme_env_of btenv in
            let btenv = Env.extend_by_env tenv btenv in
            let (_, subst, btyp) = typ_exp btenv subst body in
            let subst, btyp' = iter subst cond_typ t in
            match Subst.unify subst btyp btyp' with
              Some subst -> (subst, Subst.subst_typ subst btyp)
            | None       -> err @< Printf.sprintf "%s doesn't match with %s: all branch expresions must be same types"
                                     (Type.pps_typ btyp) (Type.pps_typ btyp')
          end
        | _ -> assert false
      in
      let subst, btyp = iter subst typ branches in
      (Subst.subst_tenv subst tenv, subst, btyp)
    end
        
(* typing for let-rec *)
and typ_letrec tenv subst var funtyp exp =
  let ftyp, rtyp = Type.fresh_typvar (), Type.fresh_typvar () in
  let funtyp' = FunT (ftyp, rtyp) in
  match exp with
    Fun _ -> begin
      let tenv, subst = unify_with_tenv subst tenv funtyp funtyp' "only values which are functions can be defined recursively" in
      let funtyp = Subst.subst_typ subst funtyp in
      let tenv = Env.extend tenv var @< TypeScheme.make TypVarSet.empty funtyp in
      let tenv, subst, etyp = typ_exp tenv subst exp in
      let tenv, subst = unify_with_tenv subst tenv funtyp etyp "expression's type doesn't cossrespond with a function type" in
      (Env.remove tenv var, subst, Subst.subst_typ subst etyp)
    end
  | _    -> err "only values which are functions can be defined recursively"
    
(* typing for program *)
let typing tenv =
  
  let return tenv var (_, _, typ) =
    Env.extend tenv var @< TypeScheme.closure typ tenv, var, typ
  in
  
  function
    Exp exp -> return tenv "it" @< typ_exp tenv Subst.empty exp
  | Decl (var, exp) -> return tenv var @< typ_exp tenv Subst.empty exp
  | DeclRec (var, typ, exp) -> return tenv var @< typ_letrec tenv Subst.empty var typ exp
  | EOF -> assert false
