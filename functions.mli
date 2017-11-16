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

val add : Kernel_function.t -> unit
val first_stmt : int -> stmt
val return_stmt : int -> stmt
val res_expression : int -> exp
val formals : int -> varinfo list
val name : int -> string
val init_simulations : location -> global list
val ids : unit -> int list
val simulation : int -> varinfo

val add_pc_steps : int -> unit

val precondition : int -> predicate list
val postcondition : int -> predicate list

val add_requires : int -> predicate -> unit
val add_requires_thread : int-> (term -> predicate) -> unit

val add_ensures : int -> predicate -> unit
val add_ensures_thread : int -> (term -> predicate) -> unit
