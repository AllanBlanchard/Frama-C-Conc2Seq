open Cil_types

module Fmap = Map.Make(struct type t = int let compare = compare end)

let functions = ref Fmap.empty
let stack_init = ref Fmap.empty

let find_id kf =
  Kernel_function.get_id kf
let find_var kf =
  Kernel_function.get_vi kf
let in_old f p = Query.sload f p


let build_init func =
  let name = "init_formals_" ^ func.vname in
  let typ = TFun(Cil.voidType, (Some ["th", Cil.uintType, []]), false, []) in
  let decl = Cil.makeGlobalVar name typ in
  Cil.setFormalsDecl decl typ ;
  let spec = Cil.empty_funspec () in 
  Globals.Functions.replace_by_declaration spec decl (Cil.CurrentLoc.get());
  decl, spec

let add kf =
  let id = in_old find_id kf in
  functions := Fmap.add id kf !functions ;
  let vi = in_old find_var kf in
  stack_init := Fmap.add id (build_init vi) !stack_init

let force_get id =
  if Fmap.mem id !functions then
    Fmap.find id !functions
  else
    assert false

let first_stmt id =
  in_old Kernel_function.find_first_stmt (force_get id)

let res_expression id =
  let stmt = in_old Kernel_function.find_return (force_get id) in
  match stmt.skind with
  | Return(Some(e), _) -> e
  | _                  -> assert false

let formals id =
  in_old Kernel_function.get_formals (force_get id)

let init_simulations loc =
  List.map
    (fun (_, (vi, spec)) -> GFunDecl(spec, vi, loc))
    (Fmap.bindings !stack_init)

let ids () =
  Fmap.fold (fun k _ l -> k :: l) !stack_init []

let simulation id =
  if Fmap.mem id !stack_init then
    let (vi, _) = Fmap.find id !stack_init in vi
  else
    assert false
