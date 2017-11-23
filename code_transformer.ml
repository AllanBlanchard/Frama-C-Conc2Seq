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
open Errors

let partial_visitor = ref None

let get_new_function vi =
  let kf = Globals.Functions.get vi in
  Kernel_function.get_vi kf

class base_type = Visitor.frama_c_copy
class expr_visitor prj th loc = object(me)
  inherit base_type prj

  val th = Cil.evar th
  
  method private pr_vexp  e  = Visitor.visitFramacExpr (me :> base_type) e
  method private pr_vlval lv = Visitor.visitFramacLval (me :> base_type) lv
  
  method! vstmt_aux s =
    match s.skind with
    | Instr(Local_init(vi,AssignInit(SingleInit(e)),loc)) ->
      let s = Cil.mkStmt(Instr(Set( (Var(vi), NoOffset), e, loc))) in
      Cil.ChangeDoChildrenPost (s, fun s -> s)
    | Instr(Call(res, fct, l ,loc)) when Specified_atomic.call_stmt s ->
      let res = match res with
        | None   -> None
        | Some lv -> Some (me#pr_vlval lv)
      in
      let fct = match fct.enode with
        | Lval( (Var vi), NoOffset ) ->
          Cil.new_exp ~loc (Lval( (Var (get_new_function vi)), NoOffset ))
        | _ -> assert false
      in
      let l = List.map (me#pr_vexp) l in
      Cil.ChangeTo(Cil.mkStmt(Instr(Call(res, fct, l, loc))))
    | Instr(Local_init(vi, ConsInit(fct, l, _), loc)) ->
      let res = Some (Vars.get_c_access_to vi.vid ~th:(Some th) ~no:NoOffset loc) in
      let fct = Cil.new_exp ~loc (Lval( (Var (get_new_function fct)), NoOffset)) in
      let l = List.map (me#pr_vexp) l in
      Cil.ChangeTo(Cil.mkStmt(Instr(Call(res, fct, l, loc))))
    | Instr(Call(_, _, _ ,_)) ->
      raise (BadConstruct "Non atomic calls from atomic block")
    | Block(_) when Specified_atomic.stmt s ->
      raise (BadConstruct "Nested Atomic blocks")
    | _ -> Cil.DoChildren


  method! vblock _ =
    let modify b = { b with blocals = [] } in
    Cil.DoChildrenPost modify

  method! vlval _ = 
    let modify (host, offset) =
      match host with
      | Var(vi) when Thread_local.variable vi ->
        Vars.get_c_access_to vi.vid ~th:(Some th) ~no:offset loc
      | Var(vi) when vi.vglob ->
        Vars.get_c_access_to vi.vid ~th:(None   ) ~no:offset loc
      | Var(vi) ->
        Vars.get_c_access_to vi.vid ~th:(Some th) ~no:offset loc
      | _       ->
        host, offset
    in
    Cil.DoChildrenPost modify
end

let visitor prj th loc =
  match !partial_visitor with
  | None ->
    let v = new expr_visitor prj in
    partial_visitor := Some v ;
    v th loc
  | Some v -> v th loc
