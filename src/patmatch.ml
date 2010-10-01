open OptionMonad
open Misc
open Common
open Syntax
open Value
open Type
open Typeexp
open Printtype
open Subst

exception Matching_error of string
let err s = raise (Matching_error (Printf.sprintf "Matching error: %s" s))

let ematch_const v c =
  match (v, c) with
    IntV i, CInt i' when i = i'   -> true
  | BoolV b, CBool b' when b = b' -> true
  | ListV [], CNullList _         -> true
  | _, _                          -> false

let rec ematch v = function
    PVar var -> Some (Env.extend Env.empty var v)
  | PWildCard -> Some Env.empty
  | PConst c -> if ematch_const v c then Some Env.empty else None
  | PAs (pat, var) -> 
      ematch v pat >>= (fun env -> Some (Env.extend env var v))
  | PCons (epat, lpat) -> begin
      match v with
        ListV (x::xs) -> begin
          let lenv = ematch (ListV xs) lpat in
          let eenv = lenv >>= fun _ -> ematch x epat in
          lenv >>= fun lenv -> eenv >>= fun eenv -> Some (Env.extend_by_env eenv lenv)
        end
      | _ -> None
    end
  | POr (lpat, rpat) -> begin
      match ematch v lpat with
        Some env -> Some env
      | _        -> ematch v rpat
    end
  | PConstr (_, pats) -> begin
      match v with
      | ConstrV (_, vs) when List.length pats = List.length vs -> begin
          OptionMonad.fold_left
            (fun env (v, pat) -> match ematch v pat with
             | None -> None | Some env' -> Some (Env.extend_by_env env env'))
            (Some Env.empty) @< List.combine vs pats
        end
      | _ -> None
    end

let rec tmatch_const tctx subst typ = function
    CInt _         -> Subst.unify subst typ PredefType.int_typ
  | CBool _        -> Subst.unify subst typ PredefType.bool_typ
  | CNullList ltyp -> begin
      let ltyp', _ = PredefType.new_listyp () in
      let ltyp'' = map_typ tctx ltyp in
      Subst.unifyl subst [(ltyp', ltyp''); (ltyp', typ)]
    end

let is_disjoint_env env env' =
  let mem, mem' = Env.members env, Env.members env' in
  let card, card' = VariableSet.cardinal mem, VariableSet.cardinal mem' in
  let sum = VariableSet.cardinal @< VariableSet.union mem mem' in
  sum = card + card'

let typs_in_tenv tenv elems =
  List.fold_right
    (fun elem typs -> match Env.lookup tenv elem with None -> assert false | Some typ -> (TypeScheme.typ typ)::typs)
    elems []

let unify_env subst env env' =
  let mem, mem' = Env.members env, Env.members env' in
  if not (VariableSet.equal mem mem') then None
  else
    let elems = VariableSet.elements mem in
    let typs = typs_in_tenv env elems in
    let typs' = typs_in_tenv env' elems in
    Subst.unifyl subst @< List.combine typs typs'

let typ_of_const tctx = function
  | CInt _      -> PredefType.int_typ
  | CBool _     -> PredefType.bool_typ
  | CNullList t -> map_typ tctx t

let rec argtyps = function
  | TyFun (atyp, rtyp)    -> atyp::(argtyps rtyp)
  | TyAlias (typ, _, _)   -> argtyps typ
  | TyVar _ | TyVariant _ -> []

let rec retyp typ = match typ with
  | TyFun (_, rtyp)       -> retyp rtyp
  | TyAlias (typ, _, _)   -> retyp typ
  | TyVar _ | TyVariant _ -> typ

let rec tmatch tctx subst typ pat : (string * TypeScheme.t) Env.t * Subst.t = match pat with
    PVar var -> (Env.extend Env.empty var @< TypeScheme.monotyp typ, subst)
  | PWildCard -> (Env.empty, subst)
  | PConst c -> begin
      match tmatch_const tctx subst typ c with
      | Some subst -> (Env.empty, subst)
      | None       -> err @< Printf.sprintf "type %s doesn't match with type %s"
                             (pps_typ typ) (pps_typ @< typ_of_const tctx c)
    end
  | PAs (pat, var) -> begin
      let tenv, subst = tmatch tctx subst typ pat in
      match Env.lookup tenv var with
        None   -> (Env.extend tenv var @< TypeScheme.monotyp typ, subst)
      | Some _ -> err @< Printf.sprintf "variable %s is bound several times" var
    end
  | PCons (epat, lpat) -> begin
      let ltyp, etyp = PredefType.new_listyp () in
      match Subst.unify subst ltyp typ with
      | None -> err "cons patterns match with only list type"
      | Some subst -> begin
          let etenv, subst = tmatch tctx subst etyp epat in
          let ltenv, subst = tmatch tctx subst ltyp lpat in
          if is_disjoint_env etenv ltenv then (Env.extend_by_env etenv ltenv, subst)
          else err "variables are bound several times"
        end
    end
  | POr (lpat, rpat) -> begin
      let ltenv, subst = tmatch tctx subst typ lpat in
      let rtenv, subst = tmatch tctx subst typ rpat in
      match unify_env subst ltenv rtenv with
      | Some subst -> (ltenv, subst)
      | None       -> err "both sieds of or-pattern must have same bindings exactly"
    end
  | PConstr (constr_name, pats) -> begin
      let pat_len = List.length pats in
      match variant_constr tctx constr_name with
      | None -> err @< Printf.sprintf "no such variant constructor %s" constr_name
      | Some (_, constr_typs)
          when List.length constr_typs <> pat_len ->
          err @< Printf.sprintf "%s has %d arguments exactly, but %d arguments was specified in the pattern"
            constr_name (List.length constr_typs) pat_len
      | Some (typdef, constr_typs) -> begin
          let ident = typdef.td_id in
          let typvars = fresh_typvar_list typdef.td_arity in
          let variant_typ = TyVariant (typvars, ident)in
          let subst = match Subst.unify subst variant_typ typ with
            | None -> err @< Printf.sprintf "type %s doesn't match with type %s"
                             (pps_typ typ) (pps_typ variant_typ)
            | Some subst -> subst
          in
          let tvmap = init_typvarmap typdef.td_params typvars in
          let constr_typs = List.map (replace_tyvar tvmap) constr_typs in
          List.fold_left (fun (tenv, subst) (typ, pat) ->
                            let tenv', subst = tmatch tctx subst typ pat in
                            if is_disjoint_env tenv tenv' then (Env.extend_by_env tenv tenv', subst)
                            else err "variables are bound several times")
            (Env.empty, subst) @< List.combine constr_typs pats
        end
    end
