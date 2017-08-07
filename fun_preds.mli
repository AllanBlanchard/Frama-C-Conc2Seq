val register: Cil_types.logic_info -> unit
val globals: Cil_types.location -> Cil_types.global list

val make_visitor: Cil_types.term -> Cil_types.location -> Visitor.frama_c_copy
