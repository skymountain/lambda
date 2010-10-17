open OptionMonad
open Misc
open Syntax
open Value
open Subst

let monotyp = TypeScheme.monotyp
let typ = TypeScheme.typ

module VarSet = Set.Make(String)

let ematch_const v c =
  match (v, c) with
    IntV i, CInt i' when i = i'   -> true
  | BoolV b, CBool b' when b = b' -> true
  | ListV [], CNullList           -> true
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
      
let mem_env env = List.fold_left (fun acc (x, _) -> VarSet.add x acc) VarSet.empty @< Env.list_of env
  
let rec tmatch err tenv subst = function
    PVar var -> if Env.mem tenv var then err @< Printf.sprintf "variable %s is bound several times in a pattern" var
    else
      let typ = Type.fresh_typvar () in
      (Env.extend tenv var @< monotyp typ, subst, typ)
  | WildCard -> (tenv, subst, Type.fresh_typvar ())
  | PConst c -> (tenv, subst, Type.of_const c)
  | As (pat, var) -> begin
      let tenv, subst, typ = tmatch err tenv subst pat in
      if Env.mem tenv var then
        err @< Printf.sprintf "variable %s is bound several times in a pattern" var
      else (Env.extend tenv var @< monotyp typ, subst, typ)
    end
  | PCons (epat, lpat) -> begin
      let tenv, subst, etyp = tmatch err tenv subst epat in
      let tenv, subst, ltyp = tmatch err tenv subst lpat in
      match unify subst ltyp @< ListT etyp with
        Some subst -> (subst_tenv subst tenv, subst, subst_typ subst ltyp)
      | None       -> err @< Printf.sprintf "cons patterns match with only values of list type, not %s"
                               @< Printtyp.pps_typ @< subst_typ subst ltyp
    end
  | POr (lpat, rpat) -> begin
      let msg = "both sieds of or-pattern must have same bindings exactly" in
      let ltenv, subst, ltyp = tmatch err tenv subst lpat in
      let rtenv, subst, rtyp = tmatch err tenv subst rpat in
      let subst = unify subst ltyp rtyp in
      let lmem, rmem = mem_env ltenv, mem_env rtenv in
      if VarSet.cardinal lmem <> VarSet.cardinal rmem then err msg
      else
        let subst = OptionMonad.fold_left
          (fun acc var -> match Env.lookup ltenv var, Env.lookup rtenv var with
             Some ltyp, Some rtyp -> unify acc (typ ltyp) (typ rtyp)
           | _, _                 -> None)
          subst @< VarSet.elements lmem
        in
        match subst with
          Some subst -> (subst_tenv subst ltenv, subst, subst_typ subst ltyp)
        | None -> err msg
    end
