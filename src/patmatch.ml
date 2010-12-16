open OptionMonad
open Misc
open Common
open Syntax
open Value
open Subst
open Types
open Type
open TypeContext
open PredefType
open Printtype
open Subst

let monotyp = TypeScheme.monotyp
let typ = TypeScheme.typ

let ematch_const v c =
  match (v, c) with
    IntV i, CInt i' when i = i'   -> true
  | BoolV b, CBool b' when b = b' -> true
  | ListV [], CNullList _         -> true
  | _, _                          -> false

let rec ematch v = function
    PVar var -> Some (Env.extend Env.empty var v)
  | WildCard -> Some Env.empty
  | PConst c -> if ematch_const v c then Some Env.empty else None
  | As (pat, var) -> 
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

let rec tmatch_const tctx subst typ const =
  let matched = match const with
    | CInt _         -> (tctx, unify subst typ PredefType.int_typ)
    | CBool _        -> (tctx, unify subst typ PredefType.bool_typ)
    | CNullList ltyp -> begin
        let tvmap, ltyp = map_typ tctx ltyp in
        let tctx = update_typvar_map tctx tvmap in
        match unify subst ltyp typ with
          Some subst -> (tctx, Some subst)
        | None -> (tctx, None)
      end
  in
  match matched with
    (_, None) -> None
  | (tctx, Some subst) -> Some (tctx, subst)

let is_disjoint_env env env' =
  let mem, mem' = Env.members env, Env.members env' in
  let card, card' = VariableSet.cardinal mem, VariableSet.cardinal mem' in
  let sum = VariableSet.cardinal @< VariableSet.union mem mem' in
  sum = card + card'

let rec tmatch err tctx subst typ = function
  | PVar var -> (Env.extend Env.empty var @< monotyp typ, typvar_map tctx, subst)
  | WildCard -> (Env.empty, typvar_map tctx, subst)
  | PConst c -> begin
      match tmatch_const tctx subst typ c with
        None -> err
          @< Printf.sprintf "type %s doesn't match with type %s"
          (pps_typ typ) (pps_typ @< snd @< Type.const_of tctx c)
      | Some (tctx, subst) -> (Env.empty, typvar_map tctx, subst)
    end
  | As (pat, var) -> begin
      let tenv, tvmap, subst = tmatch err tctx subst typ pat in
      match Env.lookup tenv var with
        None   -> (Env.extend tenv var @< monotyp typ, tvmap, subst)
      | Some _ -> err @< Printf.sprintf "variable %s is bound several times" var
    end
  | PCons (epat, lpat) -> begin
      let ltyp, etyp = list_with_new_typvar () in
      match unify subst ltyp typ with
      | Some subst -> begin
          let etenv, tvmap, subst = tmatch err tctx subst etyp epat in
          let ltenv, tvmap, subst = tmatch err (update_typvar_map tctx tvmap) subst ltyp lpat in
          if is_disjoint_env etenv ltenv then (subst_tenv subst @< Env.extend_by_env etenv ltenv, tvmap, subst)
          else err "variable %s is bound several times"
        end
      | None -> err "cons pattern can match only with list type"
    end
  | POr (lpat, rpat) -> begin
      let ltenv, tvmap, subst = tmatch err tctx subst typ lpat in
      let rtenv, tvmap, subst = tmatch err (update_typvar_map tctx tvmap) subst typ rpat in
      match unify_monotyp_env subst ltenv rtenv with
      | Some subst -> (subst_tenv subst rtenv, tvmap, subst)
      | None -> err "both sieds of or-pattern must have same bindings exactly"
    end
