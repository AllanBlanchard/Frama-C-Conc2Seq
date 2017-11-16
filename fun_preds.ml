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


module LImap = Map.Make(struct type t = int let compare = compare end)
let logic_infos = ref LImap.empty

class base_type = Visitor.frama_c_copy
class term_visitor prj th result loc = object(me)
  inherit base_type prj

  method private pr_term t = Visitor.visitFramacTerm (me :> base_type) t
  method private pr_toffset o = Visitor.visitFramacTermOffset (me :> base_type) o
  method private pr_tlval lv = Visitor.visitFramacTermLval (me :> base_type) lv
  
  method! vterm_node _ =
    let modify node =
      match node with
      | Tapp(li, lbls, params) when LImap.mem li.l_var_info.lv_id !logic_infos ->
        Tapp(LImap.find li.l_var_info.lv_id !logic_infos, lbls, th :: params)
      | _ -> node
    in Cil.DoChildrenPost modify

  method! vpredicate_node _ =
    let modify node =
      match node with
      | Papp(li, lbls, params) when LImap.mem li.l_var_info.lv_id !logic_infos ->
        Papp(LImap.find li.l_var_info.lv_id !logic_infos, lbls, th :: params)
      | _ -> node
    in Cil.DoChildrenPost modify

  method! vterm t =
    match t.term_node with
    | TLval(TResult(_), offset) ->
      let offset = me#pr_toffset offset in
      let host, off = match result with
        | None ->
          Options.failure "Result cannot be translated" ;
          assert false
        | Some lv -> me#pr_tlval lv
      in
      let noff = Logic_const.addTermOffset off offset in
      Cil.ChangeTo (Logic_const.term (TLval (host, noff)) t.term_type)
    | _ -> Cil.DoChildren

  method! vterm_lval _ =
    let modify (host, offset) =
      match host with
      | TVar(lv) ->
        begin match lv.lv_origin with
          | None -> host, offset
          | Some vi when Thread_local.is_thread_local vi ->
            Vars.l_access vi.vid ~th:(Some th) ~no:offset loc
          | Some vi when vi.vglob ->
            Vars.l_access vi.vid ~th:None ~no:offset loc
          | Some vi ->
            Vars.l_access vi.vid ~th:(Some th) ~no:offset loc
        end
      | _ -> host, offset
    in
    Cil.DoChildrenPost modify
end

let make_visitor th ?res:(res=None) loc =
  new term_visitor (Project.current()) th res loc

let transform_body body th =
  let loc = Cil.CurrentLoc.get() in
  let visitor = make_visitor th loc in
  match body with
  | LBnone ->
    LBnone
  | LBreads(terms) ->
    LBreads(List.map (Visitor.visitFramacIdTerm (visitor :> base_type)) terms)
  | LBterm(term) ->
    LBterm(Visitor.visitFramacTerm (visitor :> base_type) term)
  | LBpred(pred) ->
    LBpred(Visitor.visitFramacPredicate (visitor :> base_type) pred)
  | LBinductive(list) ->
    let change (name, lbls, subn, p) =
      (name, lbls, subn, Visitor.visitFramacPredicate (visitor :> base_type) p)
    in
    LBinductive (List.map change list)

let build_simulation li =
  let lv = Cil_const.make_logic_var_formal "th" Linteger in
  let nli = Cil_const.make_logic_info li.l_var_info.lv_name in
  let nli = {
    nli with
    l_labels  = li.l_labels ;
    l_tparams = li.l_tparams ;
    l_type    = li.l_type ;
    l_profile = lv :: li.l_profile ;
    l_body    = transform_body li.l_body (Logic_const.tvar lv)
  } in
  Logic_utils.add_logic_function nli ;
  nli
             
let register li =
  assert (Thread_local.thlocal_logic_info li) ;
  let nli = build_simulation li in
  logic_infos := LImap.add li.l_var_info.lv_id nli !logic_infos

let globals loc =
  List.map (
    fun (_, li) ->
      let g = Dfun_or_pred(li, loc) in
      Annotations.add_global Options.emitter g ;
      GAnnot(g, loc)
  ) (LImap.bindings !logic_infos)
