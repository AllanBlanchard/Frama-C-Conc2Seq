open Cil_types

val add_stmt : kernel_function -> stmt -> unit
val globals: location -> global list
val simulation: int -> fundec
val simulations: unit -> int list

val add_pc_steps: int -> unit
val process_callret_specs: (term -> term_lval option -> Visitor.frama_c_copy) -> unit

val add_requires : int -> predicate -> unit
val add_requires_thread : int-> (term -> predicate) -> unit
val add_ensures : int -> predicate -> unit
val add_ensures_thread : int -> (term -> predicate) -> unit
