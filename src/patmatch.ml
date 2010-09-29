open OptionMonad
open Misc
open Syntax
open Value

module VarSet = Set.Make(String)

let ematch_const v c =
  match (v, c) with
    IntV i, CInt i' when i = i'   -> true
  | BoolV b, CBool b' when b = b' -> true
  | ListV [], CNullList _         -> true
  | _, _                          -> false
  
let list_contents = function ListV vs -> Some vs | _ -> None

let rec ematch v = function
    PVar var -> Some (Env.extend Env.empty var v)
  | WildCard -> Some Env.empty
  | PConst c -> if ematch_const v c then Some Env.empty else None
  | As (pat, var) -> 
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
      
let mem_env env = List.fold_left (fun acc (x, _) -> VarSet.add x acc) VarSet.empty @< Env.list_of env
  
let is_disjoint_env env env' =
  let mem, mem' = mem_env env, mem_env env' in
  let card, card' = VarSet.cardinal mem, VarSet.cardinal mem' in
  let sum = VarSet.cardinal @< VarSet.union mem mem' in
  sum = card + card'
      
let rec tmatch err subst typ = function
    PVar var -> (Env.extend Env.empty var typ, subst)
  | WildCard -> (Env.empty, subst)
  | PConst c -> begin
      let ctyp = Type.of_const c in
      match Subst.unify subst typ ctyp with
        Some subst -> (Env.empty, subst)
      | None       -> err @< Printf.sprintf "cannot unify in a pattern: expected %s, but %s"
                               (Type.pps_typ typ) (Type.pps_typ ctyp)
    end
  | As (pat, var) -> begin
      let (tenv, subst) = tmatch err subst typ pat in
      if Env.mem tenv var then
        err @< Printf.sprintf "variable %s is bound several times in a pattern" var
      else
        (Env.extend tenv var typ, subst)
    end
  | PList pats -> begin
      match Subst.unify_list subst typ with
        None -> err @< Printf.sprintf "list patterns match with only values of list type, not %s"
                       @< Type.pps_typ @< Subst.subst_typ subst typ
      | Some (subst, etyp) -> begin
          List.fold_left
            (fun (acc_tenv, acc_subst) pat ->
               let tenv, subst = tmatch err acc_subst etyp pat in
               if is_disjoint_env acc_tenv tenv then (Env.extend_by_env acc_tenv tenv, subst)
               else err @< Printf.sprintf "variable %s is bound several times" "1")
            (Env.empty, subst) pats
        end
    end
  | PCons (epat, lpat) -> begin
      match Subst.unify_list subst typ with
        None -> err @< Printf.sprintf "cons patterns match with only values of list type, not %s"
                       @< Type.pps_typ @< Subst.subst_typ subst typ
      | Some (subst, etyp) -> begin
          let etenv, subst = tmatch err subst etyp epat in
          let ltenv, subst = tmatch err subst (ListT etyp) lpat in
          if is_disjoint_env etenv ltenv then (Env.extend_by_env etenv ltenv, subst)
          else err @< Printf.sprintf "variable %s is bound several times" "1"
        end
    end
  | POr (lpat, rpat) -> begin
      let msg = "both sieds of or-pattern must have same bindings exactly" in
      let ltenv, subst = tmatch err subst typ lpat in
      let rtenv, subst = tmatch err subst typ rpat in
      let member env = List.fold_left (fun acc (var, _) -> VarSet.add var acc) VarSet.empty @< Env.list_of env in
      let lmem, rmem = member ltenv, member rtenv in
      if VarSet.cardinal lmem = VarSet.cardinal rmem then
        let subst_op = OptionMonad.fold_left
          (fun acc var ->
             Env.lookup ltenv var >>=
               fun ltyp -> Env.lookup rtenv var >>=
                 fun rtyp -> Subst.unify acc ltyp rtyp)
          (Some subst) @< VarSet.elements lmem
        in
        match subst_op with Some subst -> (ltenv, subst) | None -> err msg
      else err msg
    end
