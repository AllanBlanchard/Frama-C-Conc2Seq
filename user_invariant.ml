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

let invariant = ref []

let register li  =
  let name = li.l_var_info.lv_name in
  Options.feedback "Registering user invariant: %s" name ;
  let p = match li.l_body with LBpred(p) -> p | _ -> assert false in
  let th = Cil_const.make_logic_var_quant "th" Linteger in
  let loc = Cil.CurrentLoc.get() in
  let visitor = Logic_transformer.visitor (Logic_const.tvar th) loc in
  
  let new_p = if Thread_local.predicate p then
      let p = Visitor.visitFramacPredicate visitor p in
      let valid_th = Atomic_header.valid_thread_id (Logic_const.tvar th) in
      Logic_const.pforall ([th], Logic_const.pimplies (valid_th, p))
    else
      Visitor.visitFramacPredicate visitor p
  in
  invariant := { new_p with pred_name = [ name ] } :: !invariant

let predicates () = !invariant
