open Misc
open Syntax
open Common
open OptionMonad
open Types
open Type
open TypeContext
open TypeDef
open Printtype

exception Typing_error of string
let err s = raise (Typing_error (Printf.sprintf "Typing error: %s" s))

(* unification utils *)
let unify_err s = err s

let unify_with_tctx subst tctx typ1 typ2 msg =
  match Subst.unify subst typ1 typ2 with
    None       -> unify_err msg
  | Some subst -> (Subst.subst_tctx subst tctx, subst)

let unifyl_with_tctx subst tctx typs msg =
  match Subst.unifyl subst typs with
    None       -> unify_err msg
  | Some subst -> (Subst.subst_tctx subst tctx, subst)

let return typ (tctx, subst) = (tctx, subst, Subst.subst_typ subst typ)

(* (\* typing for constant *\) *)
(* let typ_const tctx = function *)
(*     CInt _ -> (typvar_map tctx, PredefType.int_typ) *)
(*   | CBool _ -> (typvar_map tctx, PredefType.bool_typ) *)
(*   | CNullList typ -> begin *)
(*       match typ with *)
(*         NameT _ -> map_typ tctx typ *)
(*       | _       -> err "specified type isn't list type" *)
(*     end *)

(* typing for binary operator *)
let typ_binop tctx subst typ1 typ2 = function
    (Plus | Minus | Mult | Div | Lt ) -> assert false
  | Cons -> begin
      let ltyp, etyp = PredefType.list_with_new_typvar () in
      let tctx, subst, typ = return typ2 @<
        unify_with_tctx subst tctx typ2 ltyp
        @< Printf.sprintf "right-side of %s must be type %s"
        (str_of_binop Cons) (pps_typ ltyp)
      in
      let tctx, subst = unify_with_tctx subst tctx typ1 etyp
        @< Printf.sprintf "element types of %s must be same types" @< 
        str_of_binop Cons
      in
      (tctx, subst, typ)
    end

(* typing for exp *)
let rec typ_exp tctx subst = function
    Var var -> begin
      match lookup_var tctx var with
        Some t -> (tctx, subst, Subst.subst_typ subst @< TypeScheme.instantiate t)
      | None   -> err @< Printf.sprintf "%s is not bound" var
    end

  | Const c -> begin
      let tvmap, typ = Type.const_of tctx c in
      (update_typvar_map tctx tvmap, subst, typ)
    end
      
  | BinOp (op, exp1, exp2) -> begin
      let tctx, subst, typ1 = typ_exp tctx subst exp1 in
      let tctx, subst, typ2 = typ_exp tctx subst exp2 in
      typ_binop tctx subst typ1 typ2 op
    end

  | IfExp (cond, then_exp, else_exp) -> begin
      let tctx, subst, ctyp = typ_exp tctx subst cond in
      let tctx, subst = unify_with_tctx subst tctx ctyp PredefType.bool_typ "type of conditional expression must be boolean" in
      let tctx, subst, then_typ = typ_exp tctx subst then_exp in
      let tctx, subst, else_typ = typ_exp tctx subst else_exp in
      let tctx, subst = unify_with_tctx subst tctx then_typ else_typ "types of then and else expressions must be same" in
      (tctx, subst, Subst.subst_typ subst then_typ)
    end
      
  | Fun (var, typ, body) -> begin
      let tvmap, typ = map_typ tctx typ in
      let tctx = update_typvar_map tctx tvmap in
      let tctx = add_var tctx var @< TypeScheme.monotyp typ in
      let tctx, subst, rtyp = typ_exp tctx subst body in
      let tctx = remove_var tctx var in
      (tctx, subst, TyFun (Subst.subst_typ subst typ, rtyp))
    end
      
  | App (exp1, exp2) -> begin
      let tctx, subst, funtyp = typ_exp tctx subst exp1 in
      let ftyp, rtyp = Types.fresh_typvar (), Types.fresh_typvar () in
      let funtyp' = TyFun (ftyp, rtyp) in
      let tctx, subst = unify_with_tctx subst tctx funtyp funtyp' "only function type can be applied" in
      let tctx, subst, atyp = typ_exp tctx subst exp2 in
      let ftyp = Subst.subst_typ subst ftyp in
      let tctx, subst = unify_with_tctx subst tctx ftyp atyp "type of actual argument must correspond with one of formal argument" in
      (tctx, subst, Subst.subst_typ subst rtyp)
    end
      
  | Let (var, exp, body) -> begin
      let tctx, subst, typ = typ_exp tctx subst exp in
      let tctx = add_var tctx var @< TypeScheme.closure typ tctx.typ_env in
      let tctx, subst, typ = typ_exp tctx subst body in
      (remove_var tctx var, subst, typ)
    end
      
  | LetRec (var, typ, exp, body) -> begin
      let tctx, subst, typ = typ_letrec tctx subst var typ exp in
      let tctx = add_var tctx var @< TypeScheme.closure typ tctx.typ_env in
      let tctx, subst, typ = typ_exp tctx subst body in
      (remove_var tctx var, subst, typ)
    end

  (* | ListLit exps -> begin *)
  (*     let tctx, subst, typs = List.fold_right *)
  (*       (fun exp (tctx, subst, typs) -> *)
  (*          let tctx, subst, typ = typ_exp tctx subst exp in *)
  (*          (tctx, subst, typ::typs)) *)
  (*       exps (tctx, subst, []) *)
  (*     in *)
  (*     let ltyp, etyp = list_with_new_typvar () in *)
  (*     let tctx, subst = *)
  (*       unifyl_with_tctx subst tctx (List.fold_right (fun typ eqs -> (etyp, typ)::eqs) typs []) *)
  (*         @< "element types of list must be same" *)
  (*     in *)
  (*     (tctx, subst, Subst.subst_typ subst ltyp) *)
  (*   end *)

  | TypedExpr (exp, typ) -> begin
      let tvmap, typ = map_typ tctx typ in
      let tctx = update_typvar_map tctx tvmap in
      let tctx, subst, typ' = typ_exp tctx subst exp in
      let tctx, subst =
        unify_with_tctx subst tctx typ typ' "expression's type doesn't cossrespond with the specified type"
      in
      (tctx, subst, Subst.subst_typ subst typ)
    end

  | MatchExp (exp, branches) -> begin
      let tctx, subst, typ = typ_exp tctx subst exp in
      let rec iter tctx subst cond_typ = function
          [(pat, body)] -> begin
            let btenv, tvmap, subst = Patmatch.tmatch err tctx subst cond_typ pat in
            let tctx = update_typvar_map tctx tvmap in
            let btctx = extend_typ_env tctx btenv in
            let btctx, subst, btyp = typ_exp btctx subst body in
            (typvar_map btctx +> update_typvar_map tctx +> Subst.subst_tctx subst, subst, btyp)
          end
        | (pat, body)::t -> begin
            let btenv, tvmap, subst = Patmatch.tmatch err tctx subst cond_typ pat in
            let tctx = update_typvar_map tctx tvmap in
            let btctx = extend_typ_env tctx btenv in
            let btctx, subst, btyp = typ_exp btctx subst body in
            let tctx = typvar_map btctx +> update_typvar_map tctx in
            let tctx, subst, btyp' = iter (Subst.subst_tctx subst tctx) subst (Subst.subst_typ subst cond_typ) t in
            match Subst.unify subst btyp btyp' with
              Some subst -> (Subst.subst_tctx subst tctx, subst, Subst.subst_typ subst btyp)
            | None       -> err @< Printf.sprintf "%s doesn't match with %s: all branch expresions must be same types"
                                     (pps_typ btyp) (pps_typ btyp')
          end
        | _ -> assert false
      in
      iter tctx subst typ branches
    end
  | Construct _ -> assert false

(* typing for let-rec *)
and typ_letrec tctx subst var funtyp exp =
  match exp with
    Fun _ -> begin
        let ftyp, rtyp = Types.fresh_typvar (), Types.fresh_typvar () in
        let funtyp' = TyFun (ftyp, rtyp) in
        let tvmap, funtyp = map_typ tctx funtyp in
        let tctx = update_typvar_map tctx tvmap in
        let tctx, subst = unify_with_tctx subst tctx funtyp funtyp' "only values which are functions can be defined recursively" in
        let funtyp = Subst.subst_typ subst funtyp in
        let tctx = add_var tctx var @< TypeScheme.monotyp funtyp in
        let tctx, subst, etyp = typ_exp tctx subst exp in
        let tctx, subst = unify_with_tctx subst tctx funtyp etyp "expression's type doesn't cossrespond with a function type" in
        (remove_var tctx var, subst, Subst.subst_typ subst etyp)
    end
  | _ -> err "only values which are functions can be defined recursively"

(* typing for program *)
let typing tctx =
  let return var (tctx, _, typ) =
    let typ = TypeScheme.closure typ tctx.typ_env in
    let tctx = add_var tctx var typ in
    (refresh_typvar_map tctx, var, typ);
  in
  function
    Exp exp -> return "it" @< typ_exp tctx Subst.empty exp
  | Decl (var, exp) -> return var @< typ_exp tctx Subst.empty exp
  | DeclRec (var, typ, exp) -> return var @< typ_letrec tctx Subst.empty var typ exp

module StringSet = Set.Make(String)
let rec funtyp_of = function
    [] -> assert false
  | typ::[] -> typ
  | typ::typs -> TyFun (typ, (funtyp_of typs))

let define_typ tctx { Syntax.td_name = typ_name; Syntax.td_params = params; Syntax.td_kind = kind } =
  let tvmap, tvlist = List.fold_right
    (fun param (tvmap, tvlist) ->
       if TypvarMap.mem tvmap param then err "you must specify different parameters as type variables"
       else
         let tvmap = TypvarMap.add tvmap param in
         let typvar = match TypvarMap.find tvmap param with Some tv -> tv | _ -> assert false in
         (tvmap, typvar::tvlist))
    params (TypvarMap.empty, [])
  in
  let tctx = update_typvar_map tctx tvmap in
  let arity = List.length params in
  let ident = Ident.create typ_name in
  let td_kind =
    match kind with
    | Syntax.TkAlias typ -> begin
        let tvmap', typ = map_typ tctx typ in
        if not (TypvarMap.equal tvmap tvmap') then err "there are type variables which weren't specified as parameters"
        else TkAlias typ
      end
    | Syntax.TkVariant ((_::_) as constrs) -> begin
        (* XXX: recursive definition *)
        let variant_typ = TyVariant (List.map (fun x -> TyVar x) @< newtypvar_list arity, ident) in
        let _, constrs, tctx =
          List.fold_left (fun (set, constrs, tctx) (constr_name, typs) ->
                            if StringSet.mem constr_name set then err "you must specify different variant constructors"
                            else
                              let set = StringSet.add constr_name set in
                              let tctx, typs = map_typs tctx typs in
                              let constrs = (constr_name, typs)::constrs in
                              (set, constrs, update_typvar_map tctx tvmap))
            (StringSet.empty, [], tctx) constrs
        in
        if not (TypvarMap.equal (typvar_map tctx) tvmap) then err "there are type variables which weren't specified as parameters"
        else
          let constrs = List.map (fun (constr_name, typs) -> (constr_name, funtyp_of (typs @ [variant_typ]))) constrs in
          TkVariant constrs
      end
    | Syntax.TkVariant [] -> assert false
  in
  let typdef = { TypeDef.td_params = tvlist; td_arity = arity; td_kind = td_kind; td_id = ident } in
  let tctx = refresh_typvar_map tctx in
  TypeContext.insert_typ tctx ident typdef
