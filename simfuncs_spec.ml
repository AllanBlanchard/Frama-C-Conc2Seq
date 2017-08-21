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

let add_simulation_invariant () =
  let loc = Cil_datatype.Location.unknown in
  let p = Simulation_invariant.app loc in
  List.iter (fun id -> func_add_lbl_invariant id p) (Functions.ids()) ;
  List.iter (fun id -> stmt_add_lbl_invariant id p) (Statements.simulations()) ;
  Interleavings.add_invariant (p here)

let add_user_invariant () =
  let add_invariant p =
    List.iter (fun id -> func_add_invariant id p) (Functions.ids()) ;
    List.iter (fun id -> stmt_add_invariant id p) (Statements.simulations()) ;
    Interleavings.add_invariant p
  in
  List.iter add_invariant (User_invariant.predicates ())
