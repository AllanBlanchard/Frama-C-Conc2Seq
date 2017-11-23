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

class inplace = Visitor.frama_c_inplace
class copy    = Visitor.frama_c_copy

exception Name of string ;;

class name_finder = object(_)
  inherit inplace

  method! vlval lv =
    match lv with Var(vi), _ -> raise (Name vi.vname) | _ -> Cil.DoChildren
end

let find_name lv = try
    ignore Visitor.(visitFramacLval (new name_finder :> inplace) lv) ; "aux"
  with (Name s) -> s

class visitor bhv_ref prj = object(me)
  inherit copy prj

  method private pr_vexp  e  = Visitor.visitFramacExpr (me :> copy) e
  method private pr_vlval lv = Visitor.visitFramacLval (me :> copy) lv
  method private pr_voff  o  = Visitor.visitFramacOffset (me :> copy) o

  method private pr_nvi vi = Cil.memo_varinfo me#behavior vi

  val mutable insert_ph = false

  method! vfile _ =
    bhv_ref := Some me#behavior ;
    Cil.DoChildren

  method! vfunc _ =
    Cil.DoChildrenPost
      (fun f -> Cfg.clearCFGinfo ~clear_id:false f ; Cfg.cfgFun f ; f)

  method! vstmt_aux s =
    let open Atomic_spec in
    match s.skind with
    | Block(_) when Query.original atomic_stmt s ->
      Cil.JustCopy
    | UnspecifiedSequence(_) ->
      raise (Errors.BadConstruct "Unspecified sequences")
    | _ ->
      Cil.DoChildren

  method! vinst inst =
    let open Atomic_spec in
    let replace lv =
      match lv with
      | Var(vi), o -> Var(me#pr_nvi vi), me#pr_voff o
      | Mem(e), o  -> Mem(me#pr_vexp e), me#pr_voff o
    in
    let add_dummy i =
      if not (Query.original atomic_call inst)
      then [ i ; Cil.dummyInstr ] else [i]
    in
    let update_fnode efct loc =
      match efct.enode with
      | Lval(Var(fct), NoOffset) ->
        Cil.new_exp ~loc (Lval(Cil.var (me#pr_nvi fct)))
      | _ ->
        raise (Errors.BadConstruct "Function pointers")
    in
    match inst with
    | Local_init(vi, ConsInit(fvi, l, k), loc) ->
      let nvi  = me#pr_nvi vi in
      let nfvi = me#pr_nvi fvi in
      let nl   = List.map (me#pr_vexp) l in
      Cil.ChangeTo(add_dummy (Local_init(nvi, ConsInit(nfvi, nl, k), loc)))
    | Call(Some(lv), efct, l, loc) ->
      let nlv = replace lv in
      let nf = update_fnode efct loc in
      let nl = List.map (me#pr_vexp) l in
      Cil.ChangeTo(add_dummy (Call(Some(nlv), nf, nl, loc)))
    | Call(None, efct, l, loc) ->
      let nf = update_fnode efct loc in
      let nl = List.map (me#pr_vexp) l in
      Cil.ChangeTo([Call(None, nf, nl, loc)])
    | Set(lv, e, loc) ->
      Cil.ChangeTo([Set( (replace lv), (me#pr_vexp e), loc)])
    | _ ->
      Cil.DoChildren


  method! vexpr exp =
    let open Cil in
    let loc = Cil.CurrentLoc.get() in
    match exp.enode with
    | AddrOf(Var(vi), o) ->
      Cil.ChangeTo(new_exp ~loc (AddrOf (Var(me#pr_nvi vi), me#pr_voff o)))
    | StartOf(Var(vi), o) ->
      Cil.ChangeTo(new_exp ~loc (StartOf (Var(me#pr_nvi vi), me#pr_voff o)))
    | _ ->
      Cil.DoChildren


  method! vlval _ =
    let loc = Cil.CurrentLoc.get() in
    let nf = match me#current_func with
      | Some(f) -> Cil.memo_fundec me#behavior f
      | None -> assert false
    in

    (* Here the lval must be a load in a function *)
    let load id lv =
      let name = (find_name lv) ^ (string_of_int id) in
      let aux = Cil.var (Cil.makeTempVar nf ~name (Cil.typeOfLval lv)) in
      me#queueInstr [ Set(aux, (Cil.new_exp ~loc (Lval lv)), loc) ] ;
      aux
    in
    let modify lv =
      match me#current_stmt with
      | None -> lv
      | Some(stmt) ->
	begin
	  match lv with
	  | Var(v), NoOffset | Var(v), Field(_) when v.vformal -> lv 
	  | Var(v), _ when v.vglob || v.vformal -> load stmt.sid lv
	  | Mem(_), _ -> load stmt.sid lv
	  | _ -> lv
	end
    in Cil.DoChildrenPost modify
end

let make name =
  let bhv = ref None in
  let prj = File.create_project_from_visitor name (new visitor bhv) in
  match !bhv with
  | None     -> assert false
  | Some bhv -> (prj, bhv)
