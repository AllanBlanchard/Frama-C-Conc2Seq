open Cil_types

class base_type = Visitor.frama_c_inplace

exception Name of string ;;
                    
class name_finder = object(_)
  inherit base_type

  method! vlval lv =
    match lv with Var(vi), _ -> raise (Name vi.vname) | _ -> Cil.DoChildren
end
                    
class visitor = object(this)
  inherit base_type

  method private pr_vexp  e  = Visitor.visitFramacExpr (this :> base_type) e
  method private pr_vlval lv = Visitor.visitFramacLval (this :> base_type) lv
  method private pr_voff  o  = Visitor.visitFramacOffset (this :> base_type) o

  val mutable insert_ph = false

  method! vfunc _ =
    Cil.DoChildrenPost
      (fun f -> Cfg.clearCFGinfo ~clear_id:false f ; Cfg.cfgFun f ; f)
      
  method! vstmt_aux _ =
    if insert_ph then begin
        insert_ph <- false ;
        this#queueInstr [Cil.dummyInstr] ;
        Options.Self.feedback "Inserting a dummy instruction"
      end ;
    Cil.DoChildren
                          
  method! vinst inst =
    let replace_set_lvalue lv =
      match lv with
      | Var(vi), offset when vi.vglob || vi.vformal ->
         Var(vi), this#pr_voff offset
      | Var(_), _ -> lv 
      | Mem(e), o -> Mem(this#pr_vexp e), this#pr_voff o
    in
    match inst with
    | Set(lv, exp, loc) ->
       let nlv = replace_set_lvalue lv in
       let nxp = this#pr_vexp exp in
       Cil.ChangeTo ([Set(nlv, nxp, loc)])
    | Local_init(vi, ConsInit(fct, exp_list, t), loc) ->
       let nexp_list = List.map this#pr_vexp exp_list in
       insert_ph <- not (Atomic_spec.atomic_call inst) ;
       Cil.ChangeTo([Local_init(vi, ConsInit(fct, nexp_list, t), loc)])
    | Call(Some(lv), fct, exp_list, loc) ->
       let nlv = replace_set_lvalue lv in
       let nexp_list = List.map this#pr_vexp exp_list in
       insert_ph <- not (Atomic_spec.atomic_call inst) ;
       Cil.ChangeTo ([Call(Some(nlv), fct, nexp_list, loc)])
    | Call(None, fct, exp_list, loc) ->
       let nexp_list = List.map this#pr_vexp exp_list in
       Cil.ChangeTo ([Call(None, fct, nexp_list, loc)])
    | _ ->
       Cil.DoChildren
      

  method! vexpr exp =
    let loc = Cil.CurrentLoc.get() in
    match exp.enode with
    | AddrOf(Var(vi), offset) ->
      let noffset = this#pr_voff(offset) in
      Cil.ChangeTo(Cil.new_exp ~loc:loc (AddrOf (Var(vi), noffset)))
    | StartOf(Var(vi), offset) ->
      let noffset = this#pr_voff(offset) in
      Cil.ChangeTo(Cil.new_exp ~loc:loc (StartOf (Var(vi), noffset)))
    | _ -> Cil.DoChildren
             
                       
  method! vlval lv =
    let loc = Cil.CurrentLoc.get() in
    (* Here the lval must be a load in a function *)
    let load f name id lv =
      let name = name ^ (string_of_int id) in
      let aux_vi = Cil.makeTempVar f ~name:name (Cil.typeOfLval lv) in
      let aux_lv = Cil.var aux_vi in
      let aux_set = Set(aux_lv, (Cil.new_exp ~loc:loc (Lval lv)), loc) in
      this#queueInstr [aux_set] ;
      aux_lv
    in
    let name = try
        let _ = Visitor.visitFramacLval (new name_finder :> base_type) lv in "aux"
      with (Name s) -> s
    in

    let modify lv =
      match this#current_stmt with
      | None -> lv
      | Some(stmt) ->
	begin
	  let f = match this#current_func with Some(f) -> f | None -> assert false in
	  match lv with
	  | Var(v), NoOffset | Var(v), Field(_) when v.vformal ->
             lv 
	  | Var(v), _ when v.vglob || v.vformal ->
             load f name stmt.sid lv
	  | Mem(_), _ ->
             load f name stmt.sid lv
	  | _ -> lv
	end
    in Cil.DoChildrenPost modify
end

let do_visit ast =
  Visitor.visitFramacFile (new visitor) ast
                  
let make name =
  let prj = File.create_project_from_visitor name (new Visitor.frama_c_copy) in
  Old_project.initialize prj ;
  let ast = Project.on prj Ast.get() in
  Project.on prj do_visit ast ;
  prj
