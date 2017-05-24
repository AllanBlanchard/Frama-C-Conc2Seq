open Cil_types

val initialize_pc : unit -> unit
val add_global : varinfo -> initinfo -> unit
val add_local : kernel_function -> varinfo -> unit
val add_function : kernel_function -> unit

val access: int -> ?th:varinfo option -> ?no:offset -> location -> lval
                                                                    
val simulations : location -> global list
