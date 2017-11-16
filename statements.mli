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

val add_stmt : kernel_function -> stmt -> unit
val globals: location -> global list
val simulation: int -> fundec
val simulations: unit -> int list

val add_pc_steps: int -> unit
val process_callret_specs: (term -> term_lval option -> Visitor.frama_c_copy) -> unit

val add_requires : int -> predicate -> unit
val add_requires_thread : int-> (term -> predicate) -> unit
val add_ensures : int -> predicate -> unit
val add_ensures_thread : int -> (term -> predicate) -> unit
