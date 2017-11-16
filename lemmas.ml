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

let lemmas = ref []

let register name labels predicate =
  let open Logic_const in
  let th = Cil_const.make_logic_var_quant "th" Linteger in
  let loc = Cil.CurrentLoc.get() in
  let visitor = Logic_transformer.visitor (tvar th) loc in
  let p = Visitor.visitFramacPredicate visitor predicate in
  let valid_th = Atomic_header.valid_thread_id (tvar th) in
  let p = pforall ([th], pimplies (valid_th , p)) in
  lemmas := (name, labels, p) :: !lemmas

  
let globals loc =
  let globals = List.map (
      fun (name, lbls, p) ->
        let lemma = Dlemma(name, false, lbls, [], p, [], loc) in
        Annotations.add_global Options.emitter lemma ;
        GAnnot(lemma, loc)
    ) !lemmas
  in
  List.rev globals
