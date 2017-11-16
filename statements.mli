(**************************************************************************)
(*  This file is part of Conc2Seq plug-in of Frama-C.                     *)
(*                                                                        *)
(*  Copyright (C) 2016-2017 Allan Blanchard                               *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 3.                                                *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3                   *)
(*  for more details (enclosed in the file LICENCE).                      *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

val add_kf_stmt : kernel_function -> stmt -> unit
val get_located_simulation_globals: location -> global list
val get_simulation_of: int -> fundec
val get_all_ids: unit -> int list

val add_program_counter_prepost_to: int -> unit
val process_callreturn_sites_spec: (term -> term_lval option -> Visitor.frama_c_copy) -> unit

val add_requires_to : int -> predicate -> unit
val add_requires_thread_dep_to : int-> (term -> predicate) -> unit
val add_ensures_to : int -> predicate -> unit
val add_ensures_thread_dep_to : int -> (term -> predicate) -> unit
