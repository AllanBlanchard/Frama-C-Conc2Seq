open Cil_types

val initialize_pc : unit -> unit
val add_global : varinfo -> initinfo -> unit
val add_thread_local : varinfo -> initinfo -> unit
val add_local : kernel_function -> varinfo -> unit
val add_function : kernel_function -> unit

val c_access: int -> ?th:exp  option -> ?no:offset -> location -> lval
val l_memloc: int -> term -> location -> term
val l_access: int -> ?th:term option -> ?no:term_offset -> location -> term_lval
val l_ptrvalue: int -> location -> term
val sname: int -> string

val simulations : location -> global list
val ids : unit -> int list
val global_vis : unit -> varinfo list
