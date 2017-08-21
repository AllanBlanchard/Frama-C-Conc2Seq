open Cil_types

let has_thread_local_attribute attr = 
  match attr with Attr("thread_local",[]) -> true | _ -> false

let is_thread_local v = 
  List.exists has_thread_local_attribute v.vattr

exception Shame ;;

(** TODO : Add a cache to avoid multiple logic_info traversals *)

class th_detector = object(me)
  inherit Visitor.frama_c_inplace

  method! vterm t =
    match t.term_node with
    | Tapp(li,_,_) ->
      ignore(Visitor.visitFramacLogicInfo (me :> Visitor.frama_c_inplace) li) ;
      Cil.DoChildren
    | _ -> Cil.DoChildren

  method! vpredicate_node n =
    match n with
    | Papp(li,_,_) ->
      ignore(Visitor.visitFramacLogicInfo (me :> Visitor.frama_c_inplace) li) ;
      Cil.DoChildren
    | _ -> Cil.DoChildren
  
  method! vterm_lval lv =
    match lv with
    | TVar(lv), _ ->
      begin match lv.lv_origin with
      | Some v when is_thread_local v ->
        raise Shame
      | _ ->
        Cil.DoChildren
      end
    | _ -> Cil.DoChildren      
end

let thlocal_logic_info li =
  try
    ignore (Visitor.visitFramacLogicInfo (new th_detector) li) ;
    false
  with Shame -> true
  
let thlocal_predicate p =
  try
    ignore (Visitor.visitFramacPredicate (new th_detector) p) ;
    false
  with Shame -> true
    
let thlocal_term t =
  try
    ignore (Visitor.visitFramacTerm (new th_detector) t) ;
    false
  with Shame -> true

let rec thlocal_gannot ga =
  match ga with
  | Dfun_or_pred(li,_) | Dinvariant(li, _) ->
    thlocal_logic_info li
  | Dlemma(_,_,_,_, p, _,_) ->
    thlocal_predicate p
  | Daxiomatic(_, list, _, _) ->
    List.exists thlocal_gannot list
  | _ ->
    Options.Self.error "Unsupported spec type" ;
    assert false
