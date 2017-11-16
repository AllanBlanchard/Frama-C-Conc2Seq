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

val initialize_pc : unit -> unit
val add_global : varinfo -> initinfo -> unit
val add_thread_local : varinfo -> initinfo -> unit
val add_local : kernel_function -> varinfo -> unit
val add_function : kernel_function -> unit

val c_access: int -> ?th:exp  option -> ?no:offset -> location -> lval
val l_memloc: int -> term -> location -> term
val l_access: int -> ?th:term option -> ?no:term_offset -> location -> term_lval
val l_ptrvalue: int -> location -> term
val sname: int -> string

val simulations : location -> global list
val ids : unit -> int list
val global_vis : unit -> varinfo list
