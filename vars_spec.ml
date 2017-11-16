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


let make_axiom loc id =
  let vname = Vars.get_simulation_name_of id in
  Options.feedback "Building validity axiom for %s" vname ;
  let j   = Cil_const.make_logic_var_quant "j" Linteger in
  let lj  = tvar j in
  let lbl = FormalLabel("L") in
  let valid_th = Atomic_header.valid_thread_id lj in
  let simulation_location = Vars.get_logic_location_offset_of id lj loc in
  let mem_loc  = pvalid ~loc (lbl, simulation_location) in
  let tmp  = pforall([j], pimplies(valid_th, mem_loc)) in
  let impl = pimplies((Simulation_invariant.app loc lbl), tmp) in
  let name = vname ^ "_is_valid" in
  Dlemma(name, true, [lbl], [], impl, [], loc)
    
let make_range loc id =
  let max_th = Atomic_header.max_thread () in
  let up_bound = term (TBinOp (MinusA, max_th, tinteger 1)) Linteger in
  let range = trange ~loc (Some (tinteger 0), Some up_bound) in
  Vars.get_logic_location_offset_of id range loc

let make_ranges loc = 
  List.map (make_range loc) (Vars.get_all_ids())

let gvar_range loc vi =
  let open Logic_const in
  let open Logic_utils in
  match vi.vtype with
  | TInt(_) | TFloat(_) | TNamed(_) | TComp(_) | TEnum(_) ->
    let exp = Cil.new_exp ~loc (AddrOf(Var(vi), NoOffset)) in
    expr_to_term ~cast:false exp
  | TArray(typ, Some(size),_,_) ->
    let lsize = expr_to_term ~cast:true size in
    let max   = term (TBinOp(MinusA, lsize, (tinteger 1))) lsize.term_type in
    let range = trange ~loc ((Some (tinteger 0)), (Some max)) in
    let lv = Cil.cvar_to_lvar vi in
    let tlval = TVar(lv), TIndex(range, TNoOffset) in
    mk_logic_AddrOf ~loc tlval (Ctype (TPtr(typ,[])))
  | _ ->
    Options.failure "attempt to create a range for an unsupported type" ;
    assert false
    
let gvars_ranges loc =
  let (usable, unusable) = List.partition
      (fun vi ->
         match vi.vtype with
         | TArray(_,None,_,_) | TPtr(_) | TVoid(_) | TFun(_)
         | TBuiltin_va_list(_) -> false
         | _ -> true
      )(Vars.get_original_global_varinfos())
  in
  List.iter(fun vi ->
      match vi.vtype with
      | TPtr(_) ->
        Options.warning
          "%a is a pointer, separation with simulation is not currently \
           supported (ignored)"
          Cil_datatype.Varinfo.pretty vi
      | _ ->
        Options.failure "attempt to create a range for an\
                              unsupported type" ;
        assert false
    ) unusable ;
  List.map (gvar_range loc) usable

let make_separation loc =
  Options.feedback "Building axiom about simulating variables separation" ;
  let lbl = FormalLabel("L") in
  let sim_ranges = make_ranges loc in
  let glo_ranges = gvars_ranges loc in
  let p = pseparated ~loc (glo_ranges @ sim_ranges) in
  let impl = pimplies((Simulation_invariant.app loc lbl), p) in
  let name = "all_separated" in
  Dlemma(name, true, [lbl], [], impl, [], loc)

let add_vars_to_simulation_inv loc =
  let vars = List.map
      (fun i -> Vars.get_logic_location_of i loc)
      (Vars.get_all_ids ())
  in
  Simulation_invariant.reads vars
    
let get loc =
  Options.feedback "Building axiomatic block about simulating variables" ;
  let vars_axioms = List.map (make_axiom loc) (Vars.get_all_ids()) in
  let sepa = make_separation loc in
  add_vars_to_simulation_inv loc ;
  sepa :: vars_axioms
