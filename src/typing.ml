open Misc
open Syntax
open OptionMonad
open Common
open Type
open TypeContext
open Typeexp
open Printtype

(* typing for constant *)
let typ_const tctx = function
    CInt _        -> PredefType.int_typ
  | CBool _       -> PredefType.bool_typ
  | CNullList typ -> begin
      match typ with
        TName _ -> begin
          let typ = map_typ tctx typ in
          match PredefType.etyp_of_list typ with
          | Some _ -> typ
          | _      -> err "specified type isn't list type"
        end
      | _       -> err "specified type isn't list type"
    end

(* typing for binary operator *)
let typ_binop typ1 typ2 = function
    (BPlus | BMinus | BMult | BDiv | BLt ) -> assert false
  | BCons -> begin
      let ltyp = PredefType.inst_list_typ typ1 in
      if eq_typ ltyp typ2 then typ2
      else err @< Printf.sprintf "element types of %s must be same types" @< str_of_binop BCons
    end

(* typing for exp *)
let rec typ_exp tctx = function
    Var var -> begin
      match lookup_var tctx var with
        Some t -> t
      | None   -> err @< Printf.sprintf "%s is not bound" var
    end

  | Const c ->
      typ_const tctx c

  | BinOp (op, exp1, exp2) -> begin
      let typ1 = typ_exp tctx exp1 in
      let typ2 = typ_exp tctx exp2 in
      typ_binop typ1 typ2 op
    end

  | IfExp (cond, then_exp, else_exp) -> begin
      match typ_exp tctx cond with
        cond_typ when eq_typ cond_typ PredefType.bool_typ -> begin
          let then_typ = typ_exp tctx then_exp in
          let else_typ = typ_exp tctx else_exp in
          if eq_typ then_typ else_typ then then_typ
          else err "types of then and else expressions must be same"
        end
      | _ -> err "type of conditional expression must be boolean"
    end

  | Fun (var, typ, body) ->
      let typ = map_typ tctx typ in
      let tctx = add_var tctx var typ in
      let btyp = typ_exp tctx body in
      TyFun (typ, btyp)

  | App (exp1, exp2) -> begin
      match typ_exp tctx exp1 with
        TyFun (arg_typ, ret_typ) ->
          let arg_typ' = typ_exp tctx exp2 in
          if eq_typ arg_typ arg_typ' then ret_typ
          else err "type of actual argument must correspond with one of formal argument"
      | _ -> err "only function type can be applied"
    end

  | Let (var, exp, body) -> begin
      let typ = typ_exp tctx exp in
      let tctx = add_var tctx var typ in
      typ_exp tctx body
    end

  | LetRec (var, typ, exp, body) -> begin
      let typ = typ_letrec tctx var typ exp in
      let tctx = add_var tctx var typ in
      typ_exp tctx body
    end

  | ListLit exps -> begin
      let typs = List.map (typ_exp tctx) exps in
      let typ, typs = match typs with typ::typs -> (typ, typs) | _ -> assert false in (* assume exps is not empty *)
      List.iter (fun typ' -> if not (eq_typ typ typ') then err "element types of list must be same") typs;
      PredefType.inst_list_typ typ
    end

  | TypedExpr (exp, typ) -> begin
      let typ' = typ_exp tctx exp in
      let typ = map_typ tctx typ in
      if eq_typ typ typ' then typ
      else err "expression's type doesn't cossrespond with the specified type"
    end

  | MatchExp (exp, branches) -> begin
      let cond_typ = typ_exp tctx exp in
      let rec iter = function
          [(pat, body)] -> begin
            let tenv = Patmatch.tmatch tctx cond_typ pat in
            let extended_tctx = extend_typ_env tctx tenv in
            typ_exp extended_tctx  body
          end
        | (pat, body)::t -> begin
            let extended_tctx = extend_typ_env tctx @< Patmatch.tmatch tctx cond_typ pat in
            let btyp = typ_exp extended_tctx body in
            let btyp' = iter t in
            if eq_typ btyp btyp' then btyp
            else err @< Printf.sprintf "%s doesn't match with %s: all branch expresions must be same types"
              (pps_typ btyp) (pps_typ btyp')
          end
        | _ -> assert false
      in
      iter branches
    end
  | Construct (constr_name, typ) -> begin
      let rec funtyp_of =
        function
          [] -> assert false
        | typ::[] -> typ
        | typ::typs -> TyFun (typ, (funtyp_of typs))
      in
      let typ = map_typ tctx typ in
      match variant_constr tctx constr_name with
      | Some (typdef, constr_typs) -> begin
          let constr_typ = funtyp_of @<
            constr_typs @ [TyVariant (List.map (fun x -> TyVar x) typdef.td_params, typdef.td_id)]
          in
          match local_unify constr_typ typ with
          | Some _ -> typ
          | None   -> err "type of variant constructor is invalid"
        end
      | _ -> err "no such variant constructor"
    end

(* typing for let-rec *)
and typ_letrec tctx var typ exp =
  let fun_err () =
    err "only values which are functions can be defined recursively"
  in
  let rec iter typ = match typ with
    | TyFun _ -> begin
        let tctx = add_var tctx var typ in
        let etyp = typ_exp tctx exp in
        if eq_typ typ etyp then etyp
        else err "expression's type doesn't cossrespond with the specified type"
      end
    | TyAlias (typ, _, _) -> iter typ
    | _ -> fun_err ()
  in
  let typ = map_typ tctx typ in
  match exp with
  | Fun _ -> iter typ
  | _ -> fun_err ()

(* typing for program *)
let typing tctx =
  let return tctx var typ =
    let tctx = add_var tctx var typ in
    (tctx, var, typ);
  in
  function
    Exp exp -> return tctx "it" @< typ_exp tctx exp
  | Decl (var, exp) -> return tctx var @< typ_exp tctx exp
  | DeclRec (var, typ, exp) -> return tctx var @< typ_letrec tctx var typ exp


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
