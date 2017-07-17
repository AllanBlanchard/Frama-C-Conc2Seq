val prepare: Project.t -> unit
val add_sload: Cil.visitor_behavior -> Project.t -> unit
val add_simulation: Project.t -> unit

val original  : ('a -> 'b) -> 'a -> 'b
val sload     : ('a -> 'b) -> 'a -> 'b
val simulation: ('a -> 'b) -> 'a -> 'b

(*
val get_stmt_id : t -> Project.t -> int
val get_varinfo_id : t -> Project.t -> int
*)
