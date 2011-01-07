open Misc
open Typevar
open Type
open TypeContext
open MonotypevarMap

exception Typing_error of string
let err s = raise (Typing_error (Printf.sprintf "Typing error: %s" s))

let rec replace_tyvar map = function
  | TyFun (atyp, rtyp)          -> TyFun (replace_tyvar map atyp, replace_tyvar map rtyp)
  | TyVar tv                    -> begin try TypvarMap.find tv map with Not_found -> TyVar tv end
  | TyVariant (typs, typdef)    -> TyVariant (List.map (replace_tyvar map) typs, typdef)
  | TyAlias (typ, typs, typdef) -> TyAlias (replace_tyvar map typ, List.map (replace_tyvar map) typs, typdef)

let rec map_typ tctx = function
  | Syntax.TFun (arg, ret)    -> begin
      let arg = map_typ tctx arg in
      let ret = map_typ tctx ret in
      TyFun (arg, ret)
    end
  | Syntax.TVar typvar        -> begin
      TyVar (MonotypevarMap.add typvar)
    end
  | Syntax.TName (typs, name) -> begin
      match TypeContext.lookup_typ tctx name with
      | None -> err @< Printf.sprintf "%s is used as constructor name, which wasn't defined" name
      | Some (_, typdef) when typdef.td_arity <> List.length typs ->
          err @< Printf.sprintf "%s requires %d type parameters exactly, but %d type parameters was passed"
                   name typdef.td_arity @< List.length typs
      | Some (ident, typdef) -> begin
          let typs = map_typs tctx typs in
          match typdef.td_kind with
          | TkVariant _ -> TyVariant (typs, ident)
          | TkAlias typ -> begin
              let map = init_typvarmap typdef.td_params typs in
              TyAlias (replace_tyvar map typ, typs, ident)
            end
        end
    end
and map_typs tctx = List.map (map_typ tctx)

(* equality function *)
let rec eq_typ typ1 typ2 =
  match typ1, typ2 with
  | TyFun (arg1, ret1), TyFun (arg2, ret2) ->
      eq_typ arg1 arg2 && eq_typ ret1 ret2
  | TyVar tv1, TyVar tv2 -> tv1 = tv2
  | TyVariant (typs1, ident1), TyVariant (typs2, ident2) -> begin
      Ident.equal ident1 ident2 &&
        List.fold_left
          (fun acc (typ1, typ2) -> acc && eq_typ typ1 typ2)
            true (List.combine typs1 typs2)
    end
  | (TyAlias (atyp, _, _), typ) | (typ, TyAlias (atyp, _, _)) -> eq_typ atyp typ
  | (TyFun _|TyVar _|TyVariant _), _ -> false

let rec variant = function
  | TyFun _ | TyVar _       -> None
  | TyAlias (typ, _, _)     -> variant typ
  | TyVariant (typs, ident) -> Some (typs, ident)

let variant_constr tctx constr_name =
  match lookup_constr tctx constr_name with
  | None -> None
  | Some ({ td_kind = TkVariant constrs } as typdef) -> begin
      let constr_typ = List.assoc constr_name constrs in
      Some (typdef, constr_typ)
    end
  | Some _ -> assert false

let rec local_unify acc typ1 typ2 = match typ1, typ2 with
  | TyFun (atyp1, rtyp1), TyFun (atyp2, rtyp2) -> begin
      match local_unify acc atyp1 atyp2 with
        Some acc -> local_unify acc rtyp1 rtyp2
      | None     -> None
    end
  | TyVar id, _ -> begin
      if TypvarMap.mem id acc then
        if eq_typ typ2 @< TypvarMap.find id acc then Some acc
        else None
      else Some (TypvarMap.add id typ2 acc)
    end
  | TyVariant (typs1, ident1), TyVariant (typs2, ident2) -> begin
      if not (Ident.equal ident1 ident2) then None
      else
        OptionMonad.fold_left
          (fun acc (typ1, typ2) -> local_unify acc typ1 typ2)
          (Some acc) (List.combine typs1 typs2)
    end
  | TyAlias (atyp, _, _), typ | typ, TyAlias (atyp, _, _) ->
      local_unify acc atyp typ
  | (TyFun _|TyVariant _), _ -> None

let local_unify ?(init = TypvarMap.empty) typ1 typ2 = local_unify init typ1 typ2
