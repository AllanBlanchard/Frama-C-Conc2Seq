open Cil_types

let get loc =
  let sim  = Simulation_invariant.get loc in
  let vars = Vars_spec.get loc in
  let daxiom = Daxiomatic("Simulation_axioms", sim::vars, [], loc) in
  Annotations.add_global Options.emitter daxiom ;
  daxiom
