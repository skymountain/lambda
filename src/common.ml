open Types

module TypVarSet = Set.Make(struct
                              type t = typvar
                              let compare = compare
                            end)

module VariableSet = Set.Make(String)
