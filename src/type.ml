open Misc
open Types
open TypeDef
open TypeContext

let rec map_typ tctx = function
  | Syntax.FunT (arg, ret)    -> begin
      let (tvmap, arg) = map_typ tctx arg in
      let (tvmap, ret) = map_typ (update_typvar_map tctx tvmap) ret in
      (tvmap, TyFun (arg, ret))
    end
  | Syntax.VarT typvar        -> begin
      let tctx = add_typvar tctx typvar in
      match lookup_typvar tctx typvar with
        None -> assert false
      | Some id -> (typvar_map tctx, TyVar id)
    end
  | Syntax.NameT (typs, name) -> begin
      match TypeContext.lookup_typ tctx name with
      | None ->
          assert false (* XXX *)
      | Some (_, typdef) when typdef.td_arity <> List.length typs ->
          assert false (* XXX *)
      | Some (ident, typdef) -> begin
          let tctx, typs = map_typs tctx typs in
          match typdef.td_kind with
          | TkVariant _ -> (typvar_map tctx, TyVariant (typs, ident))
          | TkAlias typ -> (typvar_map tctx, TyAlias (replace_tyvar (List.combine typdef.td_params typs) typ, typs, ident))
        end
    end
and map_typs tctx typs =
  List.fold_right
    (fun typ (tctx, typs) ->
       let tvmap, typ = map_typ tctx typ in
       (update_typvar_map tctx tvmap, typ::typs))
    typs (tctx, [])

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
