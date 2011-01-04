open Misc
open Type
open Typeexp

let inst typdef typs =
  if List.length typs <> typdef.td_arity then assert false
  else match typdef.td_kind with
    TkVariant _ -> TyVariant (typs, typdef.td_id)
  | TkAlias typ      -> begin
      let map = init_typvarmap typdef.td_params typs in
      TyAlias (replace_tyvar map typ, typs, typdef.td_id)
    end

let int_ident   = Ident.create "int"
let int_typdef  = { td_params = []; td_arity = 0; td_kind = TkVariant []; td_id = int_ident; }
let int_typ = inst int_typdef []

let bool_ident  = Ident.create "bool"
let bool_typdef = { td_params = []; td_arity = 0; td_kind = TkVariant []; td_id = bool_ident; }
let bool_typ = inst bool_typdef []

let list_ident  = Ident.create "list"
let list_typdef =
  let list_etypvar = newtypvar () in
  let typvar = TyVar list_etypvar in
  { td_params   = [list_etypvar]; td_arity = 1;
    td_kind     = TkVariant [
      ("[]", []);
      ("::", [typvar])
    ];
    td_id       = list_ident; }
let inst_list_typ etyp = inst list_typdef [etyp]

let etyp_of_list typ = match variant typ with
    Some (typ::[], id) -> if Ident.equal id list_ident then Some typ else None
  | Some _            -> assert false
  | None              -> None

let predef_env = Env.extendl Env.empty [(int_ident, int_typdef); (bool_ident, bool_typdef); (list_ident, list_typdef)]
