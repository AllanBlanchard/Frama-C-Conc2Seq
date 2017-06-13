open Cil_types

class empty_project prj = object(_)
  inherit Visitor.frama_c_copy prj

  method! vglob_aux g =
    let open Globals.Functions in
    match g with
    | GVar(_) | GVarDecl(_) ->
       let modify _ = [] in Cil.DoChildrenPost modify
    | GFun(f, _) when not (Atomic_spec.atomic_fct (get f.svar)) ->
       let modify _ = [] in Cil.DoChildrenPost modify
    | GFunDecl(_, vi, _) when not (Atomic_spec.atomic_fct (get vi)) ->
       let modify _ = [] in Cil.DoChildrenPost modify
    | GAnnot(Dinvariant(_),_) ->
       let modify _ = [] in Cil.DoChildrenPost modify
    | _ ->
       Cil.JustCopy
end

let collect_globals () =
  let collect vi ii l = (vi,ii) :: l in
  Globals.Vars.fold collect []
 
let collect_locals () =
  let collect kf l =
    match kf.fundec with
    | Declaration(_) -> l
    | Definition(f, _) ->
       let r = List.fold_left (fun ll v -> (kf,v) :: ll) l f.slocals in
       List.fold_left (fun ll v -> (kf,v) :: ll) r f.sformals
  in
  Globals.Functions.fold collect []

let collect_functions () =
  let collect kf l =
    match kf.fundec with
    | Declaration(_) ->
       l
    | Definition(_, _) when Atomic_spec.atomic_fct kf ->
       l
    | _ ->
       kf :: l
  in
  Globals.Functions.fold collect []

class stmt_collector = object(this)
  inherit Visitor.frama_c_inplace
    
  val mutable stmt_list = []

  method! vstmt_aux stmt =
    let kf = match this#current_kf with None -> assert false | Some kf -> kf in
    match stmt.skind with
    | Goto(_) | Continue(_) | Break(_) | Instr(Skip(_)) 
    | Instr(Asm(_)) | Block(_) when not (Atomic_spec.atomic_stmt stmt) ->
      Cil.DoChildren
    | Block(_) when Atomic_spec.atomic_stmt stmt ->
      stmt_list <- (kf, stmt) :: stmt_list ; Cil.SkipChildren
    | _ ->
      stmt_list <- (kf, stmt) :: stmt_list ; Cil.DoChildren

  method get_list () = List.rev stmt_list
end

let collect_stmts () =
  let open Visitor in
  let collector = new stmt_collector in
  
  let collect kf =
    match kf.fundec with
    | Declaration(_) -> ()
    | Definition(fundec, _) when not (Atomic_spec.atomic_fct kf) ->
       let _ = visitFramacFunction (collector :> frama_c_inplace) fundec in ()
    | _ -> ()
  in
  Globals.Functions.iter collect ;
  collector#get_list()
                    
class visitor old_prj = object(_)
  inherit Visitor.frama_c_inplace

  method! vfile _ =
    Old_project.initialize old_prj ;
    Vars.initialize_pc () ;
    let globals = Project.on old_prj collect_globals () in
    List.iter (fun (v,ii) -> Vars.add_global v ii) globals ;
    let locals = Project.on old_prj collect_locals () in
    List.iter (fun (f,v ) -> Vars.add_local f v) locals ;
    let functions = Project.on old_prj collect_functions () in
    List.iter (fun f -> Vars.add_function f) functions ;
    List.iter (fun f -> Functions.add f) functions ;
    let statements = Project.on old_prj collect_stmts () in
    List.iter (fun (kf, s) -> Statements.add_stmt kf s) statements ;
    
    let loc = Cil.CurrentLoc.get() in
    let vglobals = Vars.simulations loc in
    let fglobals = Statements.globals loc in
    let iglobals = Functions.init_simulations loc in
    let ilv = Interleavings.get_function loc in
    let choose = Interleavings.get_choose loc in
    
    let modify f =
      f.globals <-
        vglobals
        @ f.globals
        @ iglobals
        @ fglobals
        @ [choose ; ilv] ;
      f
    in
    Cil.DoChildrenPost modify

  val mutable in_atomic_func = false
  method! vglob_aux g =
    let open Globals.Functions in
    begin match g with
    | GFun(f, _) when Atomic_spec.atomic_fct (get f.svar) ->
       in_atomic_func <- true
    | GFunDecl(_, vi, _) when Atomic_spec.atomic_fct (get vi) ->
       in_atomic_func <- true
    | _ -> ()
    end ;
    let modify v = in_atomic_func <- false ; v in
    Cil.DoChildrenPost modify
                       
  (* should be done somewhere else *)
  method! vlval _ =
    let loc = Cil.CurrentLoc.get() in
    let modify (host, offset) =
      match host with
      | Var(vi) when vi.vglob -> Vars.c_access vi.vid ~th:None ~no:offset loc
      | _ -> host, offset
    in
    Cil.DoChildrenPost modify

  method! vterm_lval _ =
    let loc = Cil.CurrentLoc.get() in
    let modify (host, offset) =
      match host with
      | TVar(lv) ->
         begin match lv.lv_origin with
         | None -> host, offset
         | Some vi when in_atomic_func && not vi.vglob ->
            host, offset
         | Some vi ->
            Vars.l_access vi.vid ~th:None ~no:offset loc
         end
      | _ -> host, offset
    in
    Cil.DoChildrenPost modify
end

let create_from old_prj = 
  let ast = Ast.get() in
  Visitor.visitFramacFile (new visitor old_prj) ast
                          
let make _ (*old_ast*) =
  let old_prj = Project.current () in
  let prj = File.create_project_from_visitor "Simulation" (new empty_project) in
  let _ = Project.on prj create_from old_prj in
  
  Project.on prj Ast.mark_as_changed () ;
  Project.on prj Ast.compute () ;
  prj
