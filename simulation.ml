open Cil_types

class empty_project prj = object(_)
  inherit Visitor.frama_c_copy prj

  method! vglob_aux _ =
    let modify _ = [] in
    Cil.DoChildrenPost modify
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
    | Declaration(_) -> l
    | Definition(_, _) -> kf :: l
  in
  Globals.Functions.fold collect []

class stmt_collector = object(this)
  inherit Visitor.frama_c_inplace
    
  val mutable stmt_list = []

  method! vstmt_aux stmt =
    let kf = match this#current_kf with None -> assert false | Some kf -> kf in
    match stmt.skind with
    | Goto(_) | Continue(_) | Break(_) | Instr(Skip(_)) 
    | Instr(Asm(_)) ->
(*  | Block(_) when not (Concurrency.is_atomic_stmt stmt) *) 
      Cil.DoChildren
(*  | Block(_) when Concurrency.is_atomic_stmt stmt ->
      stmt_list <- stmt :: stmt_list ; Cil.SkipChildren *)
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
    | Definition(fundec, _) ->
       let _ = visitFramacFunction (collector :> frama_c_inplace) fundec in ()
  in
  Globals.Functions.iter collect ;
  collector#get_list()
                    
class visitor old_prj = object(_)
  inherit Visitor.frama_c_inplace

  method! vfile _ =
    Code_transformer.old_project_is old_prj ;
    Vars.initialize_pc () ;
    let globals = Project.on old_prj collect_globals () in
    List.iter (fun (v,ii) -> Vars.add_global v ii) globals ;
    let locals = Project.on old_prj collect_locals () in
    List.iter (fun (f,v ) -> Vars.add_local f v) locals ;
    let functions = Project.on old_prj collect_functions () in
    List.iter (fun f -> Vars.add_function f) functions ;
    List.iter (fun f -> Functions.add old_prj f) functions ;
    let statements = Project.on old_prj collect_stmts () in
    List.iter (fun (kf, s) -> Statements.add_stmt kf s) statements ;
    
    let loc = Cil.CurrentLoc.get() in
    let vglobals = Vars.simulations loc in
    let fglobals = Statements.simulations loc in

    let modify f = f.globals <- vglobals @ fglobals ; f in
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
