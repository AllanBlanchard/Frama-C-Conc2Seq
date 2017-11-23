(**************************************************************************)
(*  This file is part of Conc2Seq plug-in of Frama-C.                     *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
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

val add_kf : Kernel_function.t -> unit
val get_first_stmt_of : int -> stmt
val get_return_stmt_of : int -> stmt
val get_return_expression_of : int -> exp
val get_formals_of : int -> varinfo list
val get_name_of : int -> string
val get_located_simulation_globals : location -> global list
val get_all_ids : unit -> int list
val get_simulation_of : int -> varinfo

val add_program_counter_prepost_to : int -> unit

val get_precondition_of : int -> predicate list
val get_postcondition_of : int -> predicate list

val add_requires_to : int -> predicate -> unit
val add_requires_thread_dep_to : int-> (term -> predicate) -> unit

val add_ensures_to : int -> predicate -> unit
val add_ensures_thread_dep_to : int -> (term -> predicate) -> unit
