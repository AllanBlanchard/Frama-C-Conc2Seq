open Cil_types

val add : Kernel_function.t -> unit
val first_stmt : int -> stmt
val return_stmt : int -> stmt
val res_expression : int -> exp
val formals : int -> varinfo list
val name : int -> string
val init_simulations : location -> global list
val ids : unit -> int list
val simulation : int -> varinfo

val add_pc_steps : int -> unit

val precondition : int -> predicate list
val postcondition : int -> predicate list

val add_requires : int -> predicate -> unit
val add_requires_thread : int-> (term -> predicate) -> unit

val add_ensures : int -> predicate -> unit
val add_ensures_thread : int -> (term -> predicate) -> unit
