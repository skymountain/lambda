open Misc
open Common
open Syntax

module TypVarMap = Map.Make(struct
                           type t = typvar
                           let compare = compare
                         end)

type t = TypVarSet.t * typ

let freevars =
  let rec iter typ acc =
    match typ with
      IntT | BoolT -> acc
    | FunT (ftyp, rtyp) -> iter ftyp acc +> iter rtyp
    | TypVar id -> TypVarSet.add id acc
    | ListT etyp -> iter etyp acc
    | RefT typ -> iter typ acc
  in
  fun typ -> iter typ TypVarSet.empty

let freevars_in_typ_scheme (bound_vars, typ) =
  let free_vars = freevars typ in
  TypVarSet.diff free_vars bound_vars

let freevars_in_typ_env tenv =
  List.fold_left (fun acc (_, t) -> TypVarSet.union acc @< freevars_in_typ_scheme t) TypVarSet.empty
  @< Env.list_of tenv

let rec is_syntactic_value = function
    Var _ | Const _ | Fun _ -> true
  | TypedExpr (exp, _) -> is_syntactic_value exp
  | _ -> false

let rec danger_vars = function
    IntT | BoolT | TypVar _ -> TypVarSet.empty
  | FunT (ltyp, rtyp) -> TypVarSet.union (freevars ltyp) @< danger_vars rtyp
  | ListT typ -> danger_vars typ
  | RefT typ -> freevars typ
      
let closure typ tenv exp =
  let typvars = 
    if is_syntactic_value exp then TypVarSet.diff (freevars typ) (freevars_in_typ_env tenv)
    else TypVarSet.diff (TypVarSet.diff (freevars typ) (danger_vars typ)) (freevars_in_typ_env tenv)
  in
  (typvars, typ)

let instantiate (bound_vars, typ) =
  let rec iter acc = function
      (IntT | BoolT) as t -> (acc, t)
    | FunT (ftyp, rtyp) ->
        let acc, ftyp = iter acc ftyp in
        let acc, rtyp = iter acc rtyp in
        (acc, FunT (ftyp, rtyp))
    | ListT etyp -> let acc, etyp = iter acc etyp in (acc, ListT etyp)
    | RefT typ -> let acc, typ = iter acc typ in (acc, RefT typ)
    | TypVar id -> begin
        try (acc, TypVarMap.find id acc) with
          Not_found -> begin
            let typvar =
              if TypVarSet.mem id bound_vars then Type.fresh_typvar ()
              else TypVar id
            in
            (TypVarMap.add id typvar acc, typvar)
          end
      end
  in
  let (_, t) = iter TypVarMap.empty typ in
  t

let monotyp typ = (TypVarSet.empty, typ)
    
let bound_typvars = fst
let typ = snd
