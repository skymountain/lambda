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
      
let rec tmatch_const typ c =
  match typ, c with
    IntT, CInt _               -> true
  | BoolT, CBool _             -> true
  | (ListT _) as typ1, CNullList typ2 -> Type.eq_typ typ1 typ2
  | _                          -> false

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
         Some typ' -> Type.eq_typ typ typ'
       | _         -> false)
    true
    
  
let typ_of_const = function
    CInt _ -> IntT
  | CBool _ -> BoolT
  | CNullList t -> t
      
let rec tmatch err typ = function
    PVar var -> Env.extend Env.empty var typ
  | WildCard -> Env.empty
  | PConst c ->
      if tmatch_const typ c then Env.empty
      else err @< Printf.sprintf "type %s doesn't match with type %s" (Type.pps_typ typ) (Type.pps_typ @< typ_of_const c)
  | As (pat, var) -> begin
      let tenv = tmatch err typ pat in
      match Env.lookup tenv var with
        None   -> Env.extend tenv var typ
      | Some _ -> err @< Printf.sprintf "variable %s is bound several times" var
    end
  | PList pats -> begin
      match typ with
        ListT etyp -> begin
          let tenvs = List.map (fun pat -> tmatch err etyp pat) pats in
          List.fold_left
            (fun acc tenv ->
               if is_disjoint_env acc tenv then Env.extend_by_env acc tenv
               else err @< Printf.sprintf "variable %s is bound several times" "1")
            Env.empty tenvs
        end
      | _ -> err "list pattrns match with only list type"
    end
  | PCons (epat, lpat) -> begin
      match typ with
        ListT etyp -> begin
          let etenv = tmatch err etyp epat in
          let ltenv = tmatch err typ lpat in
          if is_disjoint_env etenv ltenv then Env.extend_by_env etenv ltenv
          else err "variable %s is bound several times"
        end
      | _ -> err "cons patterns match with only list type"
    end
  | POr (lpat, rpat) -> begin
      let ltenv = tmatch err typ lpat in
      let rtenv = tmatch err typ rpat in
      if eq_env ltenv rtenv then ltenv
      else err "both sieds of or-pattern must have same bindings exactly"
    end
