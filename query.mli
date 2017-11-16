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
