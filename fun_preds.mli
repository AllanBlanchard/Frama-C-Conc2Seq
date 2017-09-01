open Cil_types

val register: logic_info -> unit
val globals: location -> global list

val make_visitor: term -> ?res:term_lval option -> location -> Visitor.frama_c_copy
