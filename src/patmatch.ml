open OptionMonad
open Misc
open Syntax
open Value
open Types
open Type

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

(* let rec proper_typ typ = *)
(*   match typ with *)
(*   | TyFun _             -> typ *)
(*   | TyVariant _         -> typ *)
(*   | TyVar _             -> typ *)
(*   | TyAlias (typ, _, _) -> typ *)

let rec tmatch_const tctx typ = function
    CInt _         -> eq_typ typ PredefType.int_typ
  | CBool _        -> eq_typ typ PredefType.bool_typ
  | CNullList ltyp -> begin
      let _, ltyp = map_typ tctx ltyp in
      eq_typ ltyp typ
      (* match proper_typ ltyp with *)
      (* | TyFun _ | TyVar _    -> assert false (\* XXX *\) *)
      (* | TyAlias _            -> assert false *)
      (* | TyVariant (typs, ident) -> Ident.equal ident PredefType.list_ident *)
    end

  (* match typ, c with *)
  (*   TyInt, CInt _                      -> true *)
  (* | TyBool, CBool _                    -> true *)
  (* | typ1, CNullList typ2 -> begin *)
  (*     let _, typ2 = map_typ tctx typ2 in *)
  (*     eq_typ typ1 typ2 *)
  (*   end *)
  (* | _                                       -> false *)

let mem_env env = List.fold_left (fun acc (x, _) -> VarSet.add x acc) VarSet.empty @< Env.list_of env
  
let is_disjoint_env env env' =
  let mem, mem' = mem_env env, mem_env env' in
  let card, card' = VarSet.cardinal mem, VarSet.cardinal mem' in
  let sum = VarSet.cardinal @< VarSet.union mem mem' in
  sum = card + card'

let eq_env env env' =
  let mem, mem' = mem_env env, mem_env env' in
  VarSet.equal mem mem' &&
    Env.fold env
    (fun eq (var, typ) -> eq &&
       match Env.lookup env' var with
         Some typ' -> eq_typ typ typ'
       | _         -> false)
    true
    
  
let typ_of_const tctx = function
    CInt _      -> PredefType.int_typ
  | CBool _     -> PredefType.bool_typ
  | CNullList t -> snd @< map_typ tctx t

let rec tmatch err tctx typ = function
    PVar var -> Env.extend Env.empty var typ
  | WildCard -> Env.empty
  | PConst c ->
      if tmatch_const tctx typ c then Env.empty
      else err @< Printf.sprintf "type %s doesn't match with type %s" (pps_typ typ) (pps_typ @< typ_of_const tctx c)
  | As (pat, var) -> begin
      let tenv = tmatch err tctx typ pat in
      match Env.lookup tenv var with
        None   -> Env.extend tenv var typ
      | Some _ -> err @< Printf.sprintf "variable %s is bound several times" var
    end
  | PList pats -> begin
      match PredefType.etyp_of_list typ with
      | Some etyp -> begin
          let tenvs = List.map (fun pat -> tmatch err tctx etyp pat) pats in
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
          let etenv = tmatch err tctx etyp epat in
          let ltenv = tmatch err tctx typ lpat in
          if is_disjoint_env etenv ltenv then Env.extend_by_env etenv ltenv
          else err "variable %s is bound several times"
        end
      | _ -> err "cons patterns match with only list type"
    end
  | POr (lpat, rpat) -> begin
      let ltenv = tmatch err tctx typ lpat in
      let rtenv = tmatch err tctx typ rpat in
      if eq_env ltenv rtenv then ltenv
      else err "both sieds of or-pattern must have same bindings exactly"
    end
