open Type
open Typeexp

let inst typdef typs =
  if List.length typs <> typdef.td_arity then assert false
  else match typdef.td_kind with
    TkVariant _ -> TyVariant (typs, typdef.td_id)
  | TkAlias typ      -> TyAlias (replace_tyvar (List.combine typdef.td_params typs) typ, typs, typdef.td_id)


let int_ident   = Ident.create "int"
let int_typdef  = { td_params = []; td_arity = 0; td_kind = TkVariant []; td_id = int_ident; }
let int_typ = inst int_typdef []

let bool_ident  = Ident.create "bool"
let bool_typdef = { td_params = []; td_arity = 0; td_kind = TkVariant []; td_id = bool_ident; }
let bool_typ = inst bool_typdef []

let list_ident  = Ident.create "list"
let list_etypvar = newtypvar ()
let list_typdef =
  let typvar = TyVar list_etypvar in
  { td_params   = [list_etypvar]; td_arity = 1;
    td_kind     = TkVariant [
      ("[]", []);
      ("::", [typvar; TyVariant ([typvar], list_ident)])
    ];
    td_id       = list_ident; }
let inst_list_typ etyp = replace_tyvar [(list_etypvar, etyp)] (TyVariant ([TyVar list_etypvar], list_ident))
let etyp_of_list typ = match variant typ with
    Some (typ::[], _) -> Some typ
  | Some _            -> assert false
  | None              -> None

let predef_env = Env.extendl Env.empty [(int_ident, int_typdef); (bool_ident, bool_typdef); (list_ident, list_typdef)]
