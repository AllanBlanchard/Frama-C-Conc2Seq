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

val initialize_program_counter : unit -> unit
val add_global : varinfo -> initinfo -> unit
val add_thread_local : varinfo -> initinfo -> unit
val add_kf_local : kernel_function -> varinfo -> unit
val add_function : kernel_function -> unit

val get_c_access_to: int -> ?th:exp option -> ?no:offset -> location -> lval
val get_logic_access_to: int -> ?th:term option -> ?no:term_offset -> location -> term_lval
val get_logic_location_of: int -> location -> term
val get_logic_location_offset_of: int -> term -> location -> term
val get_simulation_name_of: int -> string

val get_located_simulation_globals : location -> global list
val get_all_ids : unit -> int list
val get_original_global_varinfos : unit -> varinfo list
