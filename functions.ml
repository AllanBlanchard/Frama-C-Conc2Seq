(**************************************************************************)
(*  This file is part of Conc2Seq plug-in of Frama-C.                     *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 3.                                                *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3                   *)
(*  for more details (enclosed in the file LICENCE).                      *)
(*                                                                        *)
(**************************************************************************)

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

let force_get_old_kf id =
  if Fmap.mem id !functions then
    Fmap.find id !functions
  else
    assert false

let force_get_kf id =
  if Fmap.mem id !stack_init then
    Globals.Functions.get (Fmap.find id !stack_init)
  else
    assert false

let build_init func =
  let name = "init_formals_" ^ func.vname in
  let typ = TFun(Cil.voidType, (Some ["th", Cil.uintType, []]), false, []) in
  let decl = Cil.makeGlobalVar name typ in
  Cil.setFormalsDecl decl typ ;
  let spec = Cil.empty_funspec () in 
  Globals.Functions.replace_by_declaration spec decl (Cil.CurrentLoc.get());
  decl

let add_kf kf =
  let id = in_old find_id kf in
  functions := Fmap.add id kf !functions ;
  let kf = force_get_old_kf id in
  let vi = in_old find_var kf in
  stack_init := Fmap.add id (build_init vi) !stack_init

let get_first_stmt_of id =
  in_old Kernel_function.find_first_stmt (force_get_old_kf id)

let get_return_stmt_of id =
  in_old Kernel_function.find_return (force_get_old_kf id)

let get_return_expression_of id =
  let stmt = get_return_stmt_of id in
  match stmt.skind with
  | Return(Some(e), _) -> e
  | _                  -> assert false

let get_name_of id =
  in_old Kernel_function.get_name (force_get_old_kf id)

let get_formals_of id =
  in_old Kernel_function.get_formals (force_get_old_kf id)

let get_located_simulation_globals loc =
  List.map
    (fun (_, vi) -> GFunDecl(Cil.empty_funspec (), vi, loc))
    (Fmap.bindings !stack_init)

let get_all_ids () =
  Fmap.fold (fun k _ l -> k :: l) !stack_init []

let get_simulation_of id =
  if Fmap.mem id !stack_init then
    Fmap.find id !stack_init
  else
    assert false

let add_requires_to id p =
  let id_p = Logic_const.new_predicate p in
  Annotations.add_requires Options.emitter (force_get_kf id) [id_p]
  
let add_requires_thread_dep_to id p_from_th =
  let lth = Cil.cvar_to_lvar(th_parameter (force_get_kf id)) in
  let th = Logic_const.tlogic_coerce (Logic_const.tvar lth) Linteger in
  add_requires_to id (p_from_th th)

let add_ensures_to id p =
  let id_p = Logic_const.new_predicate p in
  Annotations.add_ensures Options.emitter (force_get_kf id) [Normal, id_p]
    
let add_ensures_thread_dep_to id p_from_th =
  let lth = Cil.cvar_to_lvar(th_parameter (force_get_kf id)) in
  let th = Logic_const.tlogic_coerce (Logic_const.tvar lth) Linteger in
  add_ensures_to id (p_from_th th)

let get_precondition_of id =
  let kf = force_get_old_kf id in
  let folder _ ip l = (Logic_const.pred_of_id_pred ip) :: l in
  let fold_requires () =
    Annotations.fold_requires folder kf Cil.default_behavior_name []
  in
  Query.sload fold_requires ()

let get_postcondition_of id =
  let kf = force_get_old_kf id in
  let folder _ (_, ip) l = (Logic_const.pred_of_id_pred ip) :: l in
  let fold_ensures () =
    Annotations.fold_ensures folder kf Cil.default_behavior_name []
  in
  Query.sload fold_ensures ()

(* Needs refactoring *)
let add_program_counter_prepost_to id =
  let open Logic_const in
  let lth = Cil.cvar_to_lvar(th_parameter (force_get_kf id)) in
  let th = Logic_const.tlogic_coerce (Logic_const.tvar lth) Linteger in
  let loc = Cil_datatype.Location.unknown in
  let pct = Vars.get_logic_access_to (-1) ~th:(Some th) loc in
  let before  = prel (Req, (term (TLval pct) Linteger), tinteger (-id)) in
  let stmt_id = (get_first_stmt_of id).sid in
  let after   = prel (Req, (term (TLval pct) Linteger), tinteger stmt_id) in
  add_requires_to id before ;
  add_ensures_to  id after
