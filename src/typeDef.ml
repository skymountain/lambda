open Types

type typdef = {
  td_params : typvar list;
  td_arity  : int;
  td_kind   : typ_kind;
  td_id     : Ident.t;
}

and typ_kind =
  | TkVariant of (string * typ) list
  | TkAlias of typ

let inst typdef typs =
    (* XXX *)
    if List.length typs <> typdef.td_arity then assert false
    else match typdef.td_kind with
      TkVariant _ -> TyVariant (typs, typdef.td_id)
    | TkAlias typ      -> TyAlias (replace_tyvar (List.combine typdef.td_params typs) typ, typs, typdef.td_id)
