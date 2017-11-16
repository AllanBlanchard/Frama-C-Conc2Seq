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
open Logic_const

let logic_info = ref None

let build () =
  let pc = Cil_const.make_logic_var_formal "pc" Linteger in
  let gen_equality n = prel (Req, tvar pc, tinteger n) in
  let sids =
    (List.map (fun i -> -i) (Functions.ids())) @ Statements.simulations()
  in
  let pred = pors ((gen_equality 0) :: List.map gen_equality sids) in
  let result = {
    (Cil_const.make_logic_info "valid_pc") with
    l_labels = [FormalLabel("L")] ;
    l_profile = [pc] ;
    l_body    = LBpred pred
  }
  in
  Logic_utils.add_logic_function result ;
  result

let force_get_pc_li () =
  match !logic_info with
  | None ->
    let li = build () in
    logic_info := Some li ;
    li
  | Some li -> li

let app pc lbl =
  let li = force_get_pc_li () in
  let pc = tlogic_coerce pc Linteger in
  Logic_const.papp (li,[lbl],[pc])

let build_invariant_li () =
  let loc = Cil_datatype.Location.unknown in
  let lth = Cil_const.make_logic_var_formal "th" Linteger in
  let th  = tvar lth in
  let lbl = FormalLabel("L") in
  let return_pc_value id =
    let t = term (TLval (Vars.l_access id ~th:(Some th) loc)) Linteger in
    app t lbl
  in
  let all_return_pc = List.map return_pc_value ((-1) :: Functions.ids()) in
  let pred = pands all_return_pc in
  let result = {
    (Cil_const.make_logic_info "valid_pcs") with
    l_labels = [lbl] ;
    l_profile = [lth] ;
    l_body    = LBpred pred
  }
  in
  Logic_utils.add_logic_function result ;
  result

let invariant_li = ref None

let force_get_invariant_li () =
  begin match !invariant_li with
    | None -> invariant_li := Some (build_invariant_li())
    | Some _ -> ()
  end ;
  match !invariant_li with Some l -> l | _ -> assert false

let globals loc =
  let pc_li = force_get_pc_li () in
  let inv_li = force_get_invariant_li () in
  let annot li = GAnnot(Dfun_or_pred(li, loc), loc) in
  List.map annot [ pc_li ; inv_li ]

let invariant lbl =
  let li = force_get_invariant_li () in
  let lj = Cil_const.make_logic_var_quant "j" Linteger in
  let j  = tvar lj in
  let valid_th = Atomic_header.valid_thread_id j in
  pforall([lj], pimplies(valid_th, papp (li, [lbl], [j])))
