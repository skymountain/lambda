type t

val empty : t

val find_typ       : t -> Ident.t -> Type.typdef option

val lookup_var     : t -> string -> TypeScheme.t option
val lookup_typ     : t -> string -> (Ident.t * Type.typdef) option
val lookup_constr  : t -> string -> Type.typdef option

val add_var        : t -> string -> TypeScheme.t -> t
val add_typ        : t -> Ident.t -> Type.typdef -> t

val extend_typ_env : t -> (string * TypeScheme.t) Env.t -> t

val typ_env        : t -> (string * TypeScheme.t) Env.t
