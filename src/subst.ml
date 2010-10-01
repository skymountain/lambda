open Misc
open Syntax
open Common
open Type
open OptionMonad

type t = (typvar * typ) list
type eq = typ * typ

let empty = []
let equations_of subst = List.fold_left (fun acc (id, typ) -> (TyVar id, typ)::acc) [] subst

let subst_in_equations id typ =
  let rec iter src_typ =
    match src_typ with
    | TyFun (ftyp, rtyp) -> TyFun (iter ftyp, iter rtyp)
    | TyVar id' -> begin
        if id = id' then typ
        else src_typ
      end
    | TyVariant (typs, ident) -> TyVariant (List.map iter typs, ident)
    | TyAlias (typ, typs, ident) ->
        TyAlias (iter typ, List.map iter typs, ident)
  in
  List.map (fun (typ1, typ2) -> (iter typ1, iter typ2))

let invalid_eq =
  let rec contain_typvar id = function
    | TyFun (ftyp, rtyp) -> contain_typvar id ftyp || contain_typvar id rtyp
    | TyVar x -> id = x
    | TyVariant (typs, _) -> List.exists (contain_typvar id) typs
    | TyAlias (typ, typs, _) -> List.exists (contain_typvar id) @< typ::typs
  in
  fun id typ ->
    match typ with
      TyVar id' when id = id' -> false
    | _ -> contain_typvar id typ

let unifyl subst eqs =
  let rec iter acc = function
      [] -> Some acc
    | eq::eqs -> begin
        match eq with
        | TyVar id, (_ as typ) | (_ as typ), TyVar id -> begin
            if invalid_eq id typ then None
            else
              let acc = (id, typ)::acc in
              let eqs = subst_in_equations id typ eqs in
              iter acc eqs
          end
            
        | (TyAlias (atyp, _, _), typ) | (typ, TyAlias (atyp, _, _)) ->
            iter acc @< (atyp, typ)::eqs

        | TyFun (ftyp1, rtyp1), TyFun (ftyp2, rtyp2) ->
            iter acc @< (ftyp1, ftyp2)::(rtyp1, rtyp2)::eqs

        | TyVariant (typs1, ident1), TyVariant (typs2, ident2)
            when Ident.equal ident1 ident2 ->
            iter acc @< (List.combine typs1 typs2) @ eqs

        | (TyFun _|TyVariant _), _ -> None
      end
  in
  (iter empty @< eqs @ (equations_of subst)) >>= (fun l -> Some (List.rev l))

let unify subst eq =
  unifyl subst [eq]

let rec subst_typvar subst id =
  let rec subst_fun src_typ id dst_typ eqs =
    match src_typ with
    | TyVar id' -> begin
        if id = id' then body dst_typ eqs
        else body src_typ eqs
      end
    | TyFun (ftyp, rtyp) ->
        TyFun (subst_fun ftyp id dst_typ eqs, subst_fun rtyp id dst_typ eqs)
    | TyVariant (typs, ident) ->
        TyVariant (subst_list_fun typs id dst_typ eqs, ident)
    | TyAlias (typ, typs, ident) ->
        TyAlias (subst_fun typ id dst_typ eqs, subst_list_fun typs id dst_typ eqs, ident)
  and subst_list_fun src_typs id dst_typ eqs =
    List.map (fun typ -> subst_fun typ id dst_typ eqs) src_typs
  and body typ = function
      [] -> typ
    | (id, dst_typ)::eqs ->
        subst_fun typ id dst_typ eqs
  in
  body (TyVar id) subst

let rec subst_typ subst = function
  | TyFun (ftyp, rtyp) -> TyFun (subst_typ subst ftyp, subst_typ subst rtyp)
  | TyVar id -> subst_typvar subst id
  | TyVariant (typs, ident) -> TyVariant (subst_typs subst typs, ident)
  | TyAlias (typ, typs, ident) -> TyAlias (subst_typ subst typ, subst_typs subst typs, ident)

and subst_typs subst typs = List.map (subst_typ subst) typs

let rec subst_type_scheme subst typ_scheme =
   let bound_typvars = TypeScheme.bound_typvars typ_scheme in
   let typ = TypeScheme.typ typ_scheme in
   let rec iter = function
     | TyFun (ftyp, rtyp) -> TyFun (iter ftyp, iter rtyp)
     | TyVar id -> begin
         if TypvarSet.mem id bound_typvars then TyVar id
         else subst_typvar subst id
       end
     | TyVariant (typs, ident) -> TyVariant (iter_list typs, ident)
     | TyAlias (typ, typs, ident) -> TyAlias (iter typ, iter_list typs, ident)
   and iter_list typs = List.map iter typs
   in
   (bound_typvars, iter typ)
     
let rec subst_tenv subst tenv =
  Env.fold tenv
    (fun acc (var, typ_scheme) ->
       Env.extend acc var @< subst_type_scheme subst typ_scheme)
    Env.empty

(* unify *)
let equations_of subst = List.fold_left (fun acc (id, typ) -> (TyVar id, typ)::acc) [] subst

let subst_in_equations id typ =
  let subst = [(id, typ)] in
  List.map (fun (typ1, typ2) -> (subst_typ subst typ1, subst_typ subst typ2))

let invalid_eq id typ =
  match typ with
    TyVar id' when id = id' -> false
  | _ -> TypvarSet.mem id @< TypeScheme.freevars typ

let unify_eqs eqs =
  let rec unify_eq acc eqs = function
    | TyVar id, (_ as typ) | (_ as typ), TyVar id ->
        if invalid_eq id typ then None
        else iter ((id, typ)::acc) @< subst_in_equations id typ eqs
    | TyFun (ftyp1, rtyp1), TyFun (ftyp2, rtyp2) ->
        iter acc @< (ftyp1, ftyp2)::(rtyp1, rtyp2)::eqs
    | TyVariant (typs1, ident1), TyVariant (typs2, ident2) -> begin
        if not (Ident.equal ident1 ident2) then None
        else iter acc @< eqs @ List.combine typs1 typs2
      end
    | (TyAlias (atyp, _, _), typ) | (typ, TyAlias (atyp, _, _)) ->
        iter acc ((atyp, typ)::eqs)
    | (TyFun _|TyVariant _), _ -> None
        
  and iter acc = function
      [] -> Some acc
    | eq::eqs -> unify_eq acc eqs eq
  in
  (iter empty eqs) >>= fun l -> Some (List.rev l)
  
let unifyl subst eqs =
  unify_eqs @< eqs @ (equations_of subst)

let unify subst typ1 typ2 =
  unifyl subst [(typ1, typ2)]

let merge subst1 subst2 = 
  unifyl subst1 @< equations_of subst2
