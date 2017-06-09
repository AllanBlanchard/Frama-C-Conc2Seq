val add : Kernel_function.t -> unit
val first_stmt : int -> Cil_types.stmt
val res_expression : int -> Cil_types.exp
val formals : int -> Cil_types.varinfo list
val init_simulations : Cil_types.location -> Cil_types.global list
val ids : unit -> int list
val simulation : int -> Cil_types.varinfo
