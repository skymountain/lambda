open Misc
open Common
open Type

type t = TypvarSet.t * Type.typ

let freevars_in_typ =
  let rec iter typ acc =
    match typ with
    | TyFun (ftyp, rtyp)  -> iter ftyp acc +> iter rtyp
    | TyVar id            -> TypvarSet.add id acc
    | TyVariant (typs, _) ->
        List.fold_left (fun acc typ -> iter typ acc) acc typs
    | TyAlias (typ, _, _) -> iter typ acc
  in
  fun typ -> iter typ TypvarSet.empty

let freevars_in_typ_scheme (bound_vars, typ) =
  let free_vars = freevars_in_typ typ in
  TypvarSet.diff free_vars bound_vars

let freevars_in_typ_env tenv =
  List.fold_left (fun acc (_, t) -> TypvarSet.union acc @< freevars_in_typ_scheme t) TypvarSet.empty
  @< Env.list_of tenv

let closure typ tenv =
  let bound_vars = TypvarSet.diff (freevars_in_typ typ) (freevars_in_typ_env tenv) in
  (bound_vars, typ)

let instantiate (bound_vars, typ) =
  let rec iter acc = function
    | TyFun (ftyp, rtyp) ->
        let acc, ftyp = iter acc ftyp in
        let acc, rtyp = iter acc rtyp in
        (acc, TyFun (ftyp, rtyp))
    | TyVar id -> begin
        try (acc, TypvarMap.find id acc) with
          Not_found -> begin
            let typvar =
              if TypvarSet.mem id bound_vars then Type.fresh_typvar ()
              else TyVar id
            in
            (TypvarMap.add id typvar acc, typvar)
          end
      end
    | TyVariant (typs, ident) -> begin
        let acc, typs = iter_list acc typs in
        (acc, TyVariant (typs, ident))
      end
    | TyAlias (typ, typs, ident) -> begin
        let acc, typ = iter acc typ in
        let acc, typs = iter_list acc typs in
        (acc, TyAlias (typ, typs, ident))
      end
  and iter_list acc typs =
    List.fold_right (fun typ (acc, typs) ->
                       let acc, typ = iter acc typ in
                       (acc, typ::typs))
      typs (acc, [])
  in
  let (_, t) = iter TypvarMap.empty typ in
  t

let make bound_vars typ =
  assert (TypvarSet.subset bound_vars @< freevars_in_typ typ);
  (bound_vars, typ)

let bound_typvars = fst
let typ = snd

let monotyp typ = make TypvarSet.empty typ
