let here = Logic_const.here_label

let func_add_invariant id p =
  Functions.add_requires id (p here) ;
  Functions.add_ensures id (p here)

(* let func_add_invariant_thread id p_from_th = *)
(*   Functions.add_requires_thread id p_from_th ; *)
(*   Functions.add_ensures_thread id p_from_th *)

let stmt_add_invariant id p =
  Statements.add_requires id (p here) ;
  Statements.add_ensures id (p here)

(* let stmt_add_invariant_thread id p_from_th = *)
(*   Statements.add_requires_thread id p_from_th ; *)
(*   Statements.add_ensures_thread id p_from_th *)

let add_th_parameter_validity () =
  let p_from_th = Atomic_header.valid_thread_id here in
  let add_to_func id = Functions.add_requires_thread id p_from_th in
  let add_to_stmt id = Statements.add_requires_thread id p_from_th in
  List.iter add_to_func (Functions.ids()) ;
  List.iter add_to_stmt (Statements.simulations())

let add_simulation_invariant () =
  let loc = Cil_datatype.Location.unknown in
  let p = Simulation_invariant.app loc in
  List.iter (fun id -> func_add_invariant id p) (Functions.ids()) ;
  List.iter (fun id -> stmt_add_invariant id p) (Statements.simulations()) ;
  Interleavings.add_invariant (p here)
