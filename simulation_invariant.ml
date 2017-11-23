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

let invariant = ref None

let build () =
  match !invariant with
  | None ->
    let i = { (Cil_const.make_logic_info "simulation") 
              with l_labels = [FormalLabel("L")] }
    in
    Logic_utils.add_logic_function i ;
    invariant := Some i ;
    i
  | Some i -> i

let gannot = ref None

let get loc =
  match !gannot with
  | None ->
    let li = build () in
    let g  = Dfun_or_pred(li, loc) in
    Annotations.add_global Options.emitter g ;
    gannot := Some g ;
    g
  | Some g -> g

let app loc lbl =
  let li = build () in
  Logic_const.papp ~loc (li, [lbl], [])

let reads l =
  let li = build () in
  li.l_body <- LBreads (List.map Logic_const.new_identified_term l)
