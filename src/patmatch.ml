open OptionMonad
open Misc
open Common
open Syntax
open Value
open Typevar
open Type
open Typeexp
open Printtype

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
  (* lefter pattern has higher precedence respect with environment *)
  | PList pats -> begin
      match v with
        ListV vs when List.length vs = List.length pats -> begin
          let envs = List.map (fun (pat, v) -> ematch v pat) @< List.combine pats vs in
          OptionMonad.fold_left
            (fun acc x -> x >>= (fun env -> Some (Env.extend_by_env acc env))) (Some Env.empty) envs
        end
      | _ -> None
    end
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
  | PConstr (_, _, pats) -> begin
      match v with
      | ConstrV (_, vs) when List.length pats = List.length vs -> begin
          OptionMonad.fold_left
            (fun env (v, pat) -> match ematch v pat with
             | None -> None | Some env' -> Some (Env.extend_by_env env env'))
            (Some Env.empty) @< List.combine vs pats
        end
      | _ -> None
    end

let rec tmatch_const tctx typ = function
    CInt _         -> eq_typ typ PredefType.int_typ
  | CBool _        -> eq_typ typ PredefType.bool_typ
  | CNullList ltyp -> begin
      let ltyp = map_typ tctx ltyp in
      eq_typ ltyp typ
    end

let is_disjoint_env env env' =
  let mem, mem' = Env.members env, Env.members env' in
  let card, card' = VariableSet.cardinal mem, VariableSet.cardinal mem' in
  let sum = VariableSet.cardinal @< VariableSet.union mem mem' in
  sum = card + card'

let eq_env env env' =
  let mem, mem' = Env.members env, Env.members env' in
  VariableSet.equal mem mem' &&
    Env.fold env
    (fun eq (var, typ) -> eq &&
       match Env.lookup env' var with
         Some typ' -> eq_typ typ typ'
       | _         -> false)
    true

let typ_of_const tctx = function
    CInt _      -> PredefType.int_typ
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

let rec tmatch tctx typ = function
    PVar var -> Env.extend Env.empty var typ
  | PWildCard -> Env.empty
  | PConst c ->
      if tmatch_const tctx typ c then Env.empty
      else err @< Printf.sprintf "type %s doesn't match with type %s" (pps_typ typ) (pps_typ @< typ_of_const tctx c)
  | PAs (pat, var) -> begin
      let tenv = tmatch tctx typ pat in
      match Env.lookup tenv var with
        None   -> Env.extend tenv var typ
      | Some _ -> err @< Printf.sprintf "variable %s is bound several times" var
    end
  | PList pats -> begin
      match PredefType.etyp_of_list typ with
      | Some etyp -> begin
          let tenvs = List.map (fun pat -> tmatch tctx etyp pat) pats in
          List.fold_left
            (fun acc tenv ->
               if is_disjoint_env acc tenv then Env.extend_by_env acc tenv
               else err @< Printf.sprintf "variable %s is bound several times" "1")
            Env.empty tenvs
        end
      | _ -> err "list pattrns match with only list type"
    end
  | PCons (epat, lpat) -> begin
      match PredefType.etyp_of_list typ with
        Some etyp -> begin
          let etenv = tmatch tctx etyp epat in
          let ltenv = tmatch tctx typ lpat in
          if is_disjoint_env etenv ltenv then Env.extend_by_env etenv ltenv
          else err "variables are bound several times"
        end
      | _ -> err "cons patterns match with only list type"
    end
  | POr (lpat, rpat) -> begin
      let ltenv = tmatch tctx typ lpat in
      let rtenv = tmatch tctx typ rpat in
      if eq_env ltenv rtenv then ltenv
      else err "both sieds of or-pattern must have same bindings exactly"
    end
  | PConstr (constr_name, typ, pats) -> begin
      let pat_len = List.length pats in
      let typ = map_typ tctx typ in
      let argtyps = argtyps typ in
      match variant_constr tctx constr_name with
      | None -> err @< Printf.sprintf "no such variant constructor %s" constr_name
      | Some (_, constr_typs)
          when List.length constr_typs <> pat_len || List.length argtyps <> pat_len ->
          let constr_typ_len = List.length constr_typs in
          err @< Printf.sprintf "%s has %d arguments exactly, but %d arguments was specified in the pattern"
            constr_name (if constr_typ_len <> pat_len then constr_typ_len else List.length argtyps) pat_len
      | Some (typdef, constr_typs) -> begin
          (match retyp typ with
           | TyVariant (_, ident) when Ident.equal typdef.td_id ident -> ()
           | _ -> err "variant constructors generate values whose type is the variant");
          let typvarmap = OptionMonad.fold_left
            (fun acc (constr_typ, typ) -> local_unify ~init:acc constr_typ typ)
            (Some TypvarMap.empty) @< List.combine constr_typs argtyps
          in
          match typvarmap with
          | None -> err "mono type variables were used polymorically"
          | Some _ -> begin
              List.fold_left (fun tenv (typ, pat) ->
                                let tenv' = tmatch tctx typ pat in
                                if is_disjoint_env tenv tenv' then Env.extend_by_env tenv tenv'
                                else err "variables are bound several times")
                Env.empty @< List.combine argtyps pats
            end
        end
    end
