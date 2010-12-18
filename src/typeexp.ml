open Misc
open Type
open TypeContext
open MonotypevarMap

exception Typing_error of string
let err s = raise (Typing_error (Printf.sprintf "Typing error: %s" s))

let rec replace_tyvar assoc = function
  | TyFun (atyp, rtyp)          -> TyFun (replace_tyvar assoc atyp, replace_tyvar assoc rtyp)
  | TyVar tv                    -> List.assoc tv assoc
  | TyVariant (typs, typdef)    -> TyVariant (List.map (replace_tyvar assoc) typs, typdef)
  | TyAlias (typ, typs, typdef) -> TyAlias (replace_tyvar assoc typ, List.map (replace_tyvar assoc) typs, typdef)

let rec map_typ tctx = function
  | Syntax.TFun (arg, ret)    -> begin
      let arg = map_typ tctx arg in
      let ret = map_typ tctx ret in
      TyFun (arg, ret)
    end
  | Syntax.TVar typvar        -> begin
      MonotypevarMap.add typvar;
      match MonotypevarMap.find typvar with
        None -> assert false
      | Some id -> TyVar id
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
          | TkAlias typ -> TyAlias (replace_tyvar (List.combine typdef.td_params typs) typ, typs, ident)
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
