val atomic_stmt : Cil_types.stmt -> bool
val atomic_fct : Kernel_function.t -> bool
val atomic_call : Cil_types.instr -> bool
val atomic_call_stmt : Cil_types.stmt -> bool
