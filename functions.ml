open Cil_types

module Fmap = Map.Make(struct type t = int let compare = compare end)

let functions = ref Fmap.empty
let stack_init = ref Fmap.empty

let find_id kf =
  Kernel_function.get_id kf
let find_var kf =
  Kernel_function.get_vi kf
let th_parameter kf =
  match Kernel_function.get_formals kf with [th] -> th | _ -> assert false

let in_old f p = Query.sload f p


let build_init func =
  let name = "init_formals_" ^ func.vname in
  let typ = TFun(Cil.voidType, (Some ["th", Cil.uintType, []]), false, []) in
  let decl = Cil.makeGlobalVar name typ in
  Cil.setFormalsDecl decl typ ;
  let spec = Cil.empty_funspec () in 
  Globals.Functions.replace_by_declaration spec decl (Cil.CurrentLoc.get());
  decl

let add kf =
  let id = in_old find_id kf in
  functions := Fmap.add id kf !functions ;
  let vi = in_old find_var kf in
  stack_init := Fmap.add id (build_init vi) !stack_init

let force_get_old id =
  if Fmap.mem id !functions then
    Fmap.find id !functions
  else
    assert false

let force_get id =
  if Fmap.mem id !stack_init then
    Globals.Functions.get (Fmap.find id !stack_init)
  else
    assert false

let first_stmt id =
  in_old Kernel_function.find_first_stmt (force_get_old id)

let res_expression id =
  let stmt = in_old Kernel_function.find_return (force_get_old id) in
  match stmt.skind with
  | Return(Some(e), _) -> e
  | _                  -> assert false

let formals id =
  in_old Kernel_function.get_formals (force_get_old id)

let init_simulations loc =
  List.map
    (fun (id, vi) ->
       let spec = Annotations.funspec (force_get_old id) in
       GFunDecl(spec, vi, loc))
    (Fmap.bindings !stack_init)

let ids () =
  Fmap.fold (fun k _ l -> k :: l) !stack_init []

let simulation id =
  if Fmap.mem id !stack_init then
    Fmap.find id !stack_init
  else
    assert false

let add_requires id p =
  let id_p = Logic_const.new_predicate p in
  Annotations.add_requires Options.emitter (force_get id) [id_p]
  
let add_requires_thread id p_from_th =
  let lth = Cil.cvar_to_lvar(th_parameter (force_get id)) in
  let th = Logic_const.tlogic_coerce (Logic_const.tvar lth) Linteger in
  add_requires id (p_from_th th)

let add_ensures id p =
  let id_p = Logic_const.new_predicate p in
  Annotations.add_ensures Options.emitter (force_get id) [Normal, id_p]
    
let add_ensures_thread id p_from_th =
  let lth = Cil.cvar_to_lvar(th_parameter (force_get id)) in
  let th = Logic_const.tlogic_coerce (Logic_const.tvar lth) Linteger in
  add_ensures id (p_from_th th)
