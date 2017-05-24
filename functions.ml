open Cil_types

module Fmap = Map.Make(struct type t = int let compare = compare end)

let functions = ref Fmap.empty

let find_id kf =
  Kernel_function.get_id kf
                    
let add old_prj kf =
  let id = Project.on old_prj find_id kf in
  functions := Fmap.add id (old_prj, kf) !functions

let force_get id =
  if Fmap.mem id !functions then
    Fmap.find id !functions
  else
    assert false

let first_stmt id =
  let (p, f) = force_get id in
  Project.on p Kernel_function.find_first_stmt f
           
let res_expression id =
  let (p, f) = force_get id in
  let stmt = Project.on p Kernel_function.find_return f in
  match stmt.skind with
  | Return(Some(e), _) -> e
  | _                  -> assert false

let formals id =
  let (p, f) = force_get id in
  Project.on p Kernel_function.get_formals f
