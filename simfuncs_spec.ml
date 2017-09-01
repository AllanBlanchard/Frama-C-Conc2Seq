open Cil_types

let here = Logic_const.here_label

let func_add_invariant id p =
  Functions.add_requires id p ;
  Functions.add_ensures id p

let func_add_lbl_invariant id p =
  func_add_invariant id (p here)

let stmt_add_invariant id p =
  Statements.add_requires id p ;
  Statements.add_ensures id p 

let stmt_add_lbl_invariant id p =
  stmt_add_invariant id (p here)

let add_th_parameter_validity () =
  let open Atomic_header in
  let add_to_func id = Functions.add_requires_thread id valid_thread_id in
  let add_to_stmt id = Statements.add_requires_thread id valid_thread_id in
  List.iter add_to_func (Functions.ids()) ;
  List.iter add_to_stmt (Statements.simulations())

let add_invariant_in_simulations i =
  List.iter (fun id -> func_add_lbl_invariant id i) (Functions.ids()) ;
  List.iter (fun id -> stmt_add_lbl_invariant id i) (Statements.simulations());
  Interleavings.add_invariant (i here)

let add_program_counter_steps () =
  List.iter Functions.add_pc_steps (Functions.ids()) ;
  List.iter Statements.add_pc_steps (Statements.simulations())

let add_simulation_invariant () =
  let loc = Cil_datatype.Location.unknown in
  add_invariant_in_simulations (Simulation_invariant.app loc) ;
  add_invariant_in_simulations (Program_counter.invariant) ;
  add_program_counter_steps ()

let add_user_invariant () =
  let add_invariant p =
    let p = { p with pred_name = "User invariant" :: p.pred_name } in
    List.iter (fun id -> func_add_invariant id p) (Functions.ids()) ;
    List.iter (fun id -> stmt_add_invariant id p) (Statements.simulations()) ;
    Interleavings.add_invariant p
  in
  List.iter add_invariant (User_invariant.predicates ())

let add_pre_for make_visitor id =
  let adapt p th =
    let name = "Original function contract" :: p.pred_name in
    let visitor = make_visitor th None in
    { (Visitor.visitFramacPredicate visitor p) with pred_name = name }
  in
  let pre = Functions.precondition id in
  List.iter (fun p -> Functions.add_ensures_thread id (adapt p)) pre

let add_prepost () =
  let loc = Cil_datatype.Location.unknown in
  let make_visitor th res = Fun_preds.make_visitor th ~res loc in
  List.iter (add_pre_for make_visitor) (Functions.ids()) ;
  Statements.process_callret_specs make_visitor
