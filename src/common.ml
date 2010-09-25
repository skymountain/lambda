open Syntax

module TypVarSet = Set.Make(struct
                              type t = typvar
                              let compare = compare
                            end)
