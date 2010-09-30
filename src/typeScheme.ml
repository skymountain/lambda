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
  in
  fun typ -> iter typ TypVarSet.empty

let freevars_in_typ_scheme (bound_vars, typ) =
  let free_vars = freevars typ in
  TypVarSet.diff free_vars bound_vars

let freevars_in_typ_env tenv =
  List.fold_left (fun acc (_, t) -> TypVarSet.union acc @< freevars_in_typ_scheme t) TypVarSet.empty
  @< Env.list_of tenv

let closure typ tenv =
  let bound_vars = TypVarSet.diff (freevars typ) (freevars_in_typ_env tenv) in
  (bound_vars, typ)

let instantiate (bound_vars, typ) =
  let rec iter acc = function
      (IntT | BoolT) as t -> (acc, t)
    | FunT (ftyp, rtyp) ->
        let acc, ftyp = iter acc ftyp in
        let acc, rtyp = iter acc rtyp in
        (acc, FunT (ftyp, rtyp))
    | ListT etyp -> let acc, etyp = iter acc etyp in (acc, ListT etyp)
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

(* let make bound_vars typ = *)
(*   assert (TypVarSet.subset bound_vars @< freevars_in_typ typ); *)
(*   (bound_vars, typ) *)

let monotyp typ = (TypVarSet.empty, typ)
    
let bound_typvars = fst
let typ = snd
