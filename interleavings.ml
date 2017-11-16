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

let choose_fun = ref None
let interleave = ref None
let loop_stmt = ref None

let build_choose_call loc =
  let name = "choose_call" in
  let decl = Cil.makeGlobalVar name (TFun(Cil.voidType, None, false, [])) in
  let typ = TFun(Cil.voidType, (Some ["th", Cil.uintType, []]), false, []) in
  Cil.setFormalsDecl decl typ ;
  let spec = Cil.empty_funspec () in
  Globals.Functions.replace_by_declaration spec decl loc;
  spec, decl

let case_identified id fvi th loc =
  let open Cil in
  let open Integer in
  let call = mkStmt (Instr(Call(None, (evar fvi), [evar th], loc))) in
  let n = Cil.new_exp ~loc:loc (Const(CInt64(of_int id, IInt, None))) in
  let call = { call with labels = [Case(n, loc)] } in
  call , [ call ; mkStmt (Break loc) ]

let case_choose vi th loc =
  case_identified 0 vi th loc

let case_stmt sid th loc =
  case_identified sid (Statements.simulation sid).svar th loc

let case_init sid th loc =
  case_identified (-sid) (Functions.simulation sid) th loc

let all_case_init th loc =
  let (starts, blocks) =
    List.fold_left (
      fun (ls, lb) id ->
        let (s, b) = case_init id th loc in
        (s :: ls) , (b :: lb)
    ) ([],[]) ( Functions.ids() ) 
  in
  starts, List.flatten blocks

let all_case_stmt th loc =
  let (starts, blocks) =
    List.fold_left (
      fun (ls, lb) id ->
        let (s, b) = case_stmt id th loc in
        (s :: ls) , (b :: lb)
    ) ([],[]) ( Statements.simulations() ) 
  in
  starts, List.flatten blocks

let switch_stmt choose th loc = 
  let schoose, ch_call = case_choose choose th loc in
  let sfuncs , f_calls = all_case_init th loc in
  let sstmts , s_calls = all_case_stmt th loc in
  let switch_b      = Cil.mkBlock (ch_call @ f_calls @ s_calls) in
  let switch_starts = schoose :: sfuncs @ sstmts in
  let access = Vars.c_access (-1) ~th:(Some (Cil.evar th)) loc in
  let pct_th = Cil.new_exp ~loc:loc (Lval (access)) in
  Cil.mkStmt( Switch(pct_th, switch_b, switch_starts, loc))

let random_thread_stmt th loc =
  let lv_th = Some( (Var th), NoOffset ) in
  let fun_vi = Kernel_function.get_vi (Atomic_header.some_thread_kf ()) in
  Cil.mkStmt (Instr (Call(lv_th, (Cil.evar fun_vi), [], loc)))

let build_code loc = 
  let open Cil in
  Options.feedback "Generating interleaving function";
  let def = Cil.emptyFunction "interleave" in
  Cil.setReturnType def Cil.voidType ;
  let th  = makeLocalVar def "th" uintType in

  let (_, decl) as f_choose = build_choose_call loc in
  choose_fun := Some f_choose ;
  (* Program_counter.spec_about f_choose 0 (map Functions.sid funcs) ; *)
  let switch   = switch_stmt decl th loc in

  let rand_init = random_thread_stmt th loc in
  let rand = random_thread_stmt th loc in
  let loop = Loop([], (mkBlock [rand ; switch]), loc, None, None) in
  let loop = mkStmt ~valid_sid:true loop in
  let ret   = mkStmt (Return(None, loc)) in
  let block = mkBlock [rand_init ; loop ; ret] in
  def.sbody <- { block with blocals = block.blocals @ def.sbody.blocals } ;
  def.slocals <- def.sbody.blocals ;
  let new_kf = { fundec = Definition(def, loc); spec = def.sspec } in
  Globals.Functions.replace_by_definition new_kf.spec def loc ;
  def.svar.vdefined <- true ;
  Cfg.clearCFGinfo def ;
  Cfg.cfgFun def ;
  interleave := Some def ;
  loop_stmt := Some loop


let build_function loc =
  build_code loc

let get_function loc =
  match !interleave with
  | None -> assert false
  | Some fd -> GFun(fd, loc)

let get_choose loc =
  match !choose_fun with
  | None -> assert false
  | Some (fs, vi) -> GFunDecl(fs, vi, loc)

let force_get r =
  match !r with None -> assert false | Some v -> v

let force_get_kf () = Globals.Functions.get (force_get interleave).svar
let force_get_choose () = Globals.Functions.get (snd (force_get choose_fun))
let force_get_loop () = force_get loop_stmt

let add_invariant p =
  let open Logic_const in
  let open Annotations in
  let kf   = force_get_kf () in
  let loop = force_get_loop () in
  let choose = force_get_choose () in
  let loop_inv = new_code_annotation (AInvariant([], true, p)) in
  
  add_requires Options.emitter kf [new_predicate p] ;
  add_code_annot Options.emitter ~kf loop loop_inv ;
  add_requires Options.emitter choose [new_predicate p] ;
  add_ensures Options.emitter choose ([Normal, new_predicate p])
