open Types
open TypeDef

let int_ident   = Ident.create "int"
let int_typdef  = { td_params = []; td_arity = 0; td_kind = TkVariant []; td_id = int_ident; }
let int_typ = inst int_typdef []

let bool_ident  = Ident.create "bool"
let bool_typdef = { td_params = []; td_arity = 0; td_kind = TkVariant []; td_id = bool_ident; }
let bool_typ = inst bool_typdef []

let list_ident  = Ident.create "list"
let list_typdef =
  let typvar    = newtypvar () in
  { td_params   = [typvar]; td_arity = 1;
    td_kind     = TkVariant [
      ("[]", TyVariant ([TyVar typvar], list_ident));
      ("::", TyFun (TyVar typvar, TyFun (TyVariant ([TyVar typvar], list_ident), TyVariant ([TyVar typvar], list_ident))));
    ];
    td_id       = list_ident; }
let rec etyp_of_list = function
    TyFun _ | TyVar _   -> None
  | TyAlias (typ, _, _) -> etyp_of_list typ
  | TyVariant (typ::[], ident) when Ident.equal ident list_ident -> Some typ
  | TyVariant (_, ident) when Ident.equal ident list_ident -> assert false
  | TyVariant _ -> None

let predef_env = Env.extendl Env.empty [(int_ident, int_typdef); (bool_ident, bool_typdef); (list_ident, list_typdef)]
