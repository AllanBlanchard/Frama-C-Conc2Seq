open Cil_types

class inplace = Visitor.frama_c_inplace
class copy    = Visitor.frama_c_copy

exception Name of string ;;

class name_finder = object(_)
  inherit inplace

  method! vlval lv =
    match lv with Var(vi), _ -> raise (Name vi.vname) | _ -> Cil.DoChildren
end

class visitor prj = object(me)
  inherit copy prj

  method private pr_vexp  e  = Visitor.visitFramacExpr (me :> copy) e
  method private pr_vlval lv = Visitor.visitFramacLval (me :> copy) lv
  method private pr_voff  o  = Visitor.visitFramacOffset (me :> copy) o

  val mutable insert_ph = false

  method! vfunc _ =
    Cil.DoChildrenPost
      (fun f -> Cfg.clearCFGinfo ~clear_id:false f ; Cfg.cfgFun f ; f)

  method! vstmt_aux _ =
    if insert_ph then begin
      insert_ph <- false ;
      me#queueInstr [Cil.dummyInstr] ;
      Options.Self.feedback "Inserting a dummy instruction"
    end ;
    Cil.DoChildren

  method! vinst inst =
    begin match inst with
      | Local_init(_, ConsInit(_, _, _), _) ->
        insert_ph <- not (Atomic_spec.atomic_call inst)
      | Call(Some(_), _, _, _) ->
        insert_ph <- not (Atomic_spec.atomic_call inst)
      | _ -> ()
    end ; Cil.DoChildren


  method! vexpr exp =
    let loc = Cil.CurrentLoc.get() in
    match exp.enode with
    | AddrOf(Var(vi), offset) ->
      let noffset = me#pr_voff(offset) in
      let nvi = Cil.memo_varinfo me#behavior vi in
      Cil.ChangeTo(Cil.new_exp ~loc:loc (AddrOf (Var(nvi), noffset)))
    | StartOf(Var(vi), offset) ->
      let noffset = me#pr_voff(offset) in
      let nvi = Cil.memo_varinfo me#behavior vi in
      Cil.ChangeTo(Cil.new_exp ~loc:loc (StartOf (Var(nvi), noffset)))
    | _ -> Cil.DoChildren


  method! vlval lv =
    let loc = Cil.CurrentLoc.get() in
    let f = match me#current_func with Some(f) -> f | None -> assert false in
    let nf = Cil.memo_fundec me#behavior f in

    (* Here the lval must be a load in a function *)
    let load name id lv =
      let name = name ^ (string_of_int id) in
      let aux_vi = Cil.makeTempVar nf ~name:name (Cil.typeOfLval lv) in
      let aux_lv = Cil.var aux_vi in
      let aux_set = Set(aux_lv, (Cil.new_exp ~loc:loc (Lval lv)), loc) in
      me#queueInstr [aux_set] ;
      aux_lv
    in
    let name = try
        let _ = Visitor.visitFramacLval (new name_finder :> inplace) lv in "aux"
      with (Name s) -> s
    in

    let modify lv =
      match me#current_stmt with
      | None -> lv
      | Some(stmt) ->
	begin
	  match lv with
	  | Var(v), NoOffset | Var(v), Field(_) when v.vformal -> lv 
	  | Var(v), _ when v.vglob || v.vformal -> load name stmt.sid lv
	  | Mem(_), _ -> load name stmt.sid lv
	  | _ -> lv
	end
    in Cil.DoChildrenPost modify
end

let make name = File.create_project_from_visitor name (new visitor) 
