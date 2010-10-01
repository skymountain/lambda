open Misc
open Syntax
open OptionMonad
open Common
open Type
open TypeContext
open Typeexp
open Printtype
open OptionMonad

let closure tctx subst typ =
  let tenv = Subst.subst_tenv subst @< TypeContext.typ_env tctx in
  TypeScheme.closure typ tenv

(* unification utils *)
let unify_err s = err s

let unify subst typ1 typ2 msg =
  match Subst.unify subst typ1 typ2 with
    None       -> unify_err msg
  | Some subst -> subst

let unifyl subst typs msg =
  match Subst.unifyl subst typs with
    None       -> unify_err msg
  | Some subst -> subst

(* typing for constant *)
let typ_const tctx subst = function
    CInt _        -> (subst, PredefType.int_typ)
  | CBool _       -> (subst, PredefType.bool_typ)
  | CNullList typ -> begin
      let typ = map_typ tctx typ in
      let ltyp, _ = PredefType.new_listyp () in
      let subst = unify subst ltyp typ "specified type isn't list type" in
      (subst, Subst.subst_typ subst ltyp)
    end

let return typ (tenv, subst) = (tenv, subst, Subst.subst_typ subst typ)
      
(* typing for binary operator *)
let typ_binop tctx subst typ1 typ2 = function
    (BPlus | BMinus | BMult | BDiv | BLt ) -> assert false
  | BCons -> begin
      let ltyp, etyp = PredefType.new_listyp () in
      let subst = unifyl subst [(ltyp, typ2); (etyp, typ1)] "element types of %s must be same types" in
      (subst, Subst.subst_typ subst ltyp)
    end

(* typing for exp *)
let rec typ_exp tctx subst = function
    Var var -> begin
      match lookup_var tctx var with
        Some t -> (subst, Subst.subst_typ subst @< TypeScheme.instantiate t)
      | None   -> err @< Printf.sprintf "%s is not bound" var
    end

  | Const c ->
      typ_const tctx subst c

  | BinOp (op, exp1, exp2) -> begin
      let subst, typ1 = typ_exp tctx subst exp1 in
      let subst, typ2 = typ_exp tctx subst exp2 in
      typ_binop tctx subst typ1 typ2 op
    end

  | IfExp (cond, then_exp, else_exp) -> begin
      let subst, ctyp = typ_exp tctx subst cond in
      let subst = unify subst ctyp PredefType.bool_typ "type of conditional expression must be boolean" in
      let subst, then_typ = typ_exp tctx subst then_exp in
      let subst, else_typ = typ_exp tctx subst else_exp in
      let subst = unify subst then_typ else_typ "types of then and else expressions must be same" in
      (subst, Subst.subst_typ subst then_typ)
    end

  | Fun (var, typ, body) -> begin
      let typ = map_typ tctx typ in
      let subst, rtyp = typ_exp (add_var tctx var @< TypeScheme.monotyp typ) subst body in
      (subst, TyFun (Subst.subst_typ subst typ, rtyp))
    end

  | App (exp1, exp2) -> begin
      let subst, funtyp = typ_exp tctx subst exp1 in
      let funtyp', ftyp, rtyp = new_funtyp () in
      let subst = unify subst funtyp funtyp' "only function type can be applied" in
      let subst, atyp = typ_exp tctx subst exp2 in
      let subst = unify subst atyp ftyp "type of actual argument must correspond with one of formal argument" in
      (subst, Subst.subst_typ subst rtyp)
    end

  | Let (var, exp, body) -> begin
      let subst, typ = typ_exp tctx subst exp in
      typ_exp (add_var tctx var @< closure tctx subst typ) subst body
    end

  | LetRec (var, typ, exp, body) -> begin
      let subst, typ = typ_letrec tctx subst var typ exp in
      typ_exp (add_var tctx var @< closure tctx subst typ) subst body
    end

  | TypedExpr (exp, typ) -> begin
      let subst, typ' = typ_exp tctx subst exp in
      let typ = map_typ tctx typ in
      let subst = unify subst typ typ' "expression's type doesn't cossrespond with the specified type" in
      (subst, Subst.subst_typ subst typ)
    end

  | MatchExp (exp, branches) -> begin
      let subst, typ = typ_exp tctx subst exp in
      let rec iter subst cond_typ = function
          [(pat, body)] -> begin
            let tenv, subst = Patmatch.tmatch tctx subst cond_typ pat in
            typ_exp (extend_typ_env tctx tenv) subst body
          end
        | (pat, body)::t -> begin
            let tenv, subst = Patmatch.tmatch tctx subst cond_typ pat in
            let subst, btyp = typ_exp (extend_typ_env tctx tenv) subst body in
            let subst, btyp' = iter subst cond_typ t in
            let subst = unify subst btyp btyp'
              @< Printf.sprintf "%s doesn't match with %s: all branch expresions must be same types"
               (pps_typ btyp) (pps_typ btyp')
            in
            (subst, Subst.subst_typ subst btyp)
          end
        | _ -> assert false
      in
      iter subst typ branches
    end

  | Construct constr_name -> begin
      let rec funtyp_of =
        function
          [] -> assert false
        | typ::[] -> typ
        | typ::typs -> TyFun (typ, (funtyp_of typs))
      in
      match variant_constr tctx constr_name with
      | Some (typdef, constr_typs) -> begin
          let constr_typ = funtyp_of @<
            constr_typs @ [TyVariant (List.map (fun x -> TyVar x) typdef.td_params, typdef.td_id)]
          in
          let typvarmap = init_typvarmap typdef.td_params @< fresh_typvar_list typdef.td_arity in
          (subst, replace_tyvar typvarmap constr_typ)
        end
      | _ -> err "no such variant constructor"
    end

(* typing for let-rec *)
and typ_letrec tctx subst var typ exp =
  let typ = map_typ tctx typ in
  let funtyp, ftyp, rtyp = new_funtyp () in
  match exp with
    Fun _ -> begin
      let subst = unify subst funtyp typ "only values which are functions can be defined recursively" in
      let tctx' = add_var tctx var @< TypeScheme.monotyp funtyp in
      let subst, etyp = typ_exp tctx' subst exp in
      let subst = unify subst funtyp etyp "expression's type doesn't cossrespond with a function type" in
      (subst, Subst.subst_typ subst etyp)
    end
  | _ -> err "only values which are functions can be defined recursively"

(* typing for program *)
let typing tctx =
  let return tctx var (subst, typ) =
    let typ = closure tctx subst typ in
    let tctx = add_var tctx var typ in
    (tctx, var, typ);
  in
  
  function
    Exp exp -> return tctx "it" @< typ_exp tctx Subst.empty exp
  | Decl (var, exp) -> return tctx var @< typ_exp tctx Subst.empty exp
  | DeclRec (var, typ, exp) -> return tctx var @< typ_letrec tctx Subst.empty var typ exp


module ConstrSet = Set.Make(String)

let define_typ tctx { Syntax.td_name = typ_name; Syntax.td_params = params; Syntax.td_kind = kind } =
  MonotypevarMap.refresh ();
  let tvlist = List.map
    (fun param ->
       if MonotypevarMap.mem param then
         err "you must specify different parameters as type variables"
       else
         MonotypevarMap.add param;
         match MonotypevarMap.find param with Some tv -> tv | _ -> assert false)
    params
  in
  let arity = List.length params in
  let ident = Ident.create typ_name in
  let typvar_num = MonotypevarMap.cardinal () in
  let td_kind =
    match kind with
    | Syntax.TkAlias typ -> begin
        let typ = map_typ tctx typ in
        TkAlias typ
      end
    | Syntax.TkVariant ((_::_) as constrs) -> begin
        let imaginary_typdef = { td_params = tvlist; td_arity = arity; td_kind = TkVariant []; td_id = ident } in
        let tctx = add_typ tctx ident imaginary_typdef in
        let _, constrs =
          List.fold_left (fun (set, constrs) (constr_name, typs) ->
                            if ConstrSet.mem constr_name set then err "you must specify different variant constructors"
                            else
                              let set = ConstrSet.add constr_name set in
                              let typs = map_typs tctx typs in
                              let constrs = (constr_name, typs)::constrs in
                              (set, constrs))
            (ConstrSet.empty, []) constrs
        in
        TkVariant constrs
      end
    | Syntax.TkVariant [] -> assert false
  in
  if typvar_num <> MonotypevarMap.cardinal () then err "there are type variables which weren't specified as parameters"
  else
    let typdef = { td_params = tvlist; td_arity = arity; td_kind = td_kind; td_id = ident } in
    TypeContext.add_typ tctx ident typdef
