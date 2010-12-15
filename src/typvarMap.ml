module M = Map.Make(struct
                      type t = string
                      let compare = String.compare
                    end)

type t = int M.t

let empty = M.empty
  
let add s map =
  if M.mem s map then map
  else begin
    M.add s (Types.newtypvar ()) map
  end

let mem k map  = M.mem k map
let find k map = M.find k map
