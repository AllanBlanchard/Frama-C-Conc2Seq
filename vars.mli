open Cil_types

val initialize_pc : unit -> unit
val add_global : varinfo -> initinfo -> unit
val add_thread_local : varinfo -> initinfo -> unit
val add_local : kernel_function -> varinfo -> unit
val add_function : kernel_function -> unit

val c_access: int -> ?th:varinfo option -> ?no:offset -> location -> lval
val l_access: int -> ?th:varinfo option -> ?no:term_offset -> location -> term_lval

val simulations : location -> global list
