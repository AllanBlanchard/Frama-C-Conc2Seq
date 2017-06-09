val add_stmt : Cil_types.kernel_function -> Cil_types.stmt -> unit
val globals: Cil_types.location -> Cil_types.global list
val simulation: int -> Cil_types.fundec
val simulations: unit -> int list
