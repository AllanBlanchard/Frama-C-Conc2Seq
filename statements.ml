open Cil_types

module Smap = Map.Make(struct type t = int let compare = compare end)

let statements = ref Smap.empty

let base_simulation name =
  Options.Self.feedback "Generating %s function" name ;
  let def = Cil.emptyFunction name in
  Cil.setReturnType def Cil.voidType ;
  let th   = Cil.makeFormalVar def "th" Cil.uintType in
  Cil.setFormals def [th] ;
  (def, th)

let finalize def body loc =
  def.sbody   <- { body with blocals = body.blocals @ def.sbody.blocals };
  def.slocals <- def.sbody.blocals ;
  let new_kf = { fundec = Definition(def, loc); spec = def.sspec } in
  Globals.Functions.replace_by_definition new_kf.spec def loc ;
  def.svar.vdefined <- true ;
  Cfg.clearCFGinfo def ;
  Cfg.cfgFun def

let affect_pc_th th exp loc = 
  let access = Vars.c_access (-1) ~th:(Some (Cil.evar th)) loc in
  Cil.mkStmt(Instr(Set(access, (Cil.new_exp loc exp), loc)))

let affect_pc_th_int th value loc =
  let const  = Const(CInt64(Integer.of_int value, IInt, None)) in
  let access = Vars.c_access (-1) ~th:(Some (Cil.evar th)) loc in
  Cil.mkStmt(Instr(Set(access, (Cil.new_exp loc const), loc)))

let affect_from fct th value loc =
  let const  = Const(CInt64(Integer.of_int value, IInt, None)) in
  let access = Vars.c_access fct.vid ~th:(Some (Cil.evar th)) loc in
  Cil.mkStmt(Instr(Set(access, (Cil.new_exp loc const), loc)))

let rec skip_skip stmt =
  match stmt.skind with
  | Continue(_) | Break(_) | Instr(Skip(_)) | Instr(Asm(_))
  | Block(_) when not (Atomic_spec.atomic_stmt stmt)
    -> skip_skip (List.hd stmt.succs)
  | Goto(r_next, _) -> skip_skip !r_next
  | _ -> stmt

let set transformer affect s =
  let next = (skip_skip (List.hd s.succs)).sid in
  let ret = affect next in
  let s = match s.skind with
    | Instr(Set(_)) -> s                         
    | Instr(Local_init(vi,AssignInit(SingleInit(e)),loc)) ->
      Cil.mkStmt(Instr(Set( (Var(vi), NoOffset), e, loc)))
    | _ -> assert false
  in
  let nstmt = Visitor.visitFramacStmt transformer s in
  [ nstmt ; ret ]

let call transformer affect fct le next th loc =
  let load v e =
    let ne = Visitor.visitFramacExpr transformer e in
    let nv = Vars.c_access v.vid ~th:(Some (Cil.evar th)) loc in
    Cil.mkStmt(Instr(Set(nv, ne, loc)))
  in
  let loads = List.map2 load (Functions.formals fct.vid) le in

  let from_stmt = affect_from fct th next loc in
  let fst_kf  = Functions.first_stmt fct.vid in
  let pc_stmt = affect fst_kf.sid in
  loads @ [from_stmt ; pc_stmt ]

let call_ret transformer affect s th =
  let dummy = List.hd s.succs in
  let next_call = dummy.sid in
  let fct, l, loc = match s.skind with
    | Instr(Call(Some(_), e, l ,loc)) ->
      begin match e.enode with
        | Lval(Var(fct), NoOffset) -> fct, l, loc
        | _                        -> assert false
      end
    | Instr(Local_init(_, ConsInit(fct, l, _), loc)) ->
      fct, l, loc
    | _ -> assert false
  in
  dummy, call transformer affect fct l next_call th loc

let atomic_call transformer affect s =
  let s = match s.skind with
    | Instr(Call(_)) -> s
    | Instr(Local_init(vi, ConsInit(fct, l, _), loc)) ->
      let lv = (Var vi), NoOffset in
      Cil.mkStmt (Instr(Call(Some(lv), Cil.evar(fct), l, loc)))
    | _ -> assert false
  in
  let stmt = Visitor.visitFramacStmt transformer s in
  let ret = affect (skip_skip (List.hd s.succs)).sid in
  [ stmt ; ret ]

let call_void transformer affect s th =
  let next_call = (skip_skip (List.hd s.succs)).sid in
  let fct, l, loc = match s.skind with
    | Instr(Call(None, e, l ,loc)) ->
      begin match e.enode with
        | Lval(Var(fct), NoOffset) -> fct, l, loc
        | _                        -> assert false
      end
    | _ -> assert false
  in
  call transformer affect fct l next_call th loc

let return kf stmt th =
  (* The call to get_vi is NOT SAFE but the way it is implemented (Silicon) *)
  (* does not depend on the global state, so it is OK there.                *) 
  let fv = Globals.Functions.get_vi kf in
  let loc  = Cil_datatype.Stmt.loc stmt in
  let from = Lval(Vars.c_access fv.vid ~th:(Some (Cil.evar th)) loc) in
  [affect_pc_th th from loc]

let cond transformer affect s loc =
  match s.skind with
  | If(e,_,_,_) ->
    let ne = Visitor.visitFramacExpr transformer e in
    let (s0, s1) = Cil.separate_if_succs s in
    let n0 = (skip_skip s0).sid and n1 = (skip_skip s1).sid in
    let r0 = affect n0          and r1 = affect n1 in
    [Cil.mkStmt (If(ne, (Cil.mkBlock [r0]), (Cil.mkBlock [r1]), loc))]
  | _ -> assert false

let switch transformer affect s loc =
  match s.skind with
  | Switch(e,_,_,_) ->
    let ne = Visitor.visitFramacExpr transformer e in
    let (lc, _) = Cil.separate_switch_succs s in
    let to_sim c =
      let next = (skip_skip c).sid in
      { (affect next) with labels = c.labels }
    in
    let nlc = List.map to_sim (List.rev lc) in
    [Cil.mkStmt (Switch(ne, (Cil.mkBlock nlc), nlc, loc))]
  | _ -> assert false

let after_block_aux s =
  match s.skind with
  | Block(b) ->
    let rec close s1 s2 =
      match List.mem b (Kernel_function.blocks_closed_by_edge s1 s2) with
      | true -> s2
      | _ -> close s2 (List.hd s2.succs)
    in close s (List.hd s.succs)
  | _ -> assert false

let after_block s =
  Query.sload after_block_aux s

let at_block transformer affect s =
  assert (Atomic_spec.atomic_stmt s) ;
  Options.Self.feedback "here";
  match s.skind with
  | Block(b) ->
    let fs = skip_skip (after_block s) in
    Options.Self.feedback "there";
    let block = Visitor.visitFramacBlock transformer b in
    let ret = affect (skip_skip fs).sid in
    let b = { block with bstmts = (block.bstmts @ [ret]) } in
    [Cil.mkStmt (Block b)]
  | _ -> assert false

let return_loading kf stmt dum =
  let ret, fct, loc = match stmt.skind with
    | Instr(Call(Some(var), e, _, loc)) ->
      begin match e.enode with
        | Lval(Var(fct), NoOffset) -> var, fct, loc
        | _                        -> assert false
      end
    | Instr(Local_init(vi,ConsInit(fct, _, _), loc)) ->
      ((Var vi), NoOffset), fct, loc
    | _ -> assert false
  in
  (* The call to get_vi is NOT SAFE but the way it is implemented (Silicon) *)
  (* does not depend on the global state, so it is OK there.                *) 
  let name = (Globals.Functions.get_vi kf).vname^ "_"^(string_of_int dum.sid) in
  let (def, th) = base_simulation name in

  let ret_exp = Functions.res_expression fct.vid in
  let transformer = Code_transformer.visitor (Project.current()) th loc in
  let ret   = Visitor.visitFramacLval transformer ret in
  let value = Visitor.visitFramacExpr transformer ret_exp in
  let create_return = Cil.mkStmt(Instr(Set(ret,value,loc))) in
  let affect = affect_pc_th_int th (skip_skip (List.hd dum.succs)).sid loc in
  let ret_stmt = Cil.mkStmt (Return (None, loc)) in
  let block = Cil.mkBlock [create_return ; affect ; ret_stmt] in
  finalize def block loc ;
  statements := Smap.add dum.sid def !statements ;
  ()

let add_stmt kf stmt =
  (* The call to get_vi is NOT SAFE but the way it is implemented (Silicon) *)
  (* does not depend on the global state, so it is OK there.                *) 
  let name = (Globals.Functions.get_vi kf).vname^"_"^(string_of_int stmt.sid) in
  let loc  = Cil_datatype.Stmt.loc stmt in
  let (def, th) = base_simulation name in
  let affect value = affect_pc_th_int th value loc in
  let transformer = Code_transformer.visitor (Project.current()) th loc in 
  let body = match stmt.skind with
    | Instr(Set(_)) | Instr(Local_init(_,AssignInit(_),_)) ->
      set transformer affect stmt
    | Instr(Call(Some(_),_,_,_)) | Instr(Local_init(_,ConsInit(_),_))
      when not(Atomic_spec.atomic_call_stmt stmt) ->
      let dum, result = call_ret transformer affect stmt th in
      return_loading kf stmt dum ;
      result
    | Instr(Call(None,_,_,_)) when not(Atomic_spec.atomic_call_stmt stmt) ->
      call_void transformer affect stmt th
    | Instr(Call(_,_,_,_)) | Instr(Local_init(_,ConsInit(_),_)) ->
      atomic_call transformer affect stmt
    | Return(_) ->
      return kf stmt th
    | If(_)     ->
      cond transformer affect stmt loc
    | Switch(_) ->
      switch transformer affect stmt loc
    | Block(_)  ->
      at_block transformer affect stmt
    | Loop(_,b,_,_,_) when b.bstmts = [] ->
      [affect stmt.sid]
    | Loop(_,b,_,_,_) ->
      [affect (skip_skip (List.hd b.bstmts)).sid]
    | _ ->
      assert false
  in
  let ret_stmt = Cil.mkStmt (Return (None, loc)) in
  let block = Cil.mkBlock (body @ [ret_stmt]) in
  finalize def block loc ;
  statements := Smap.add stmt.sid def !statements

let simulation sid =
  match Smap.mem sid !statements with
  | true -> Smap.find sid !statements
  | false -> assert false

let simulations () =
  Smap.fold (fun k _ l -> k :: l) !statements []

let globals loc =
  List.map (fun (_, f) -> GFun(f, loc)) (Smap.bindings !statements)


let force_get id =
  if Smap.mem id !statements then
    Globals.Functions.get (Smap.find id !statements).svar
  else
    assert false

let th_parameter kf =
  match Kernel_function.get_formals kf with [th] -> th | _ -> assert false

let add_requires id p =
  let id_p = Logic_const.new_predicate p in
  Annotations.add_requires Options.emitter (force_get id) [id_p]
  
let add_requires_thread id p_from_th =
  let lth = Cil.cvar_to_lvar(th_parameter (force_get id)) in
  let th = Logic_const.tlogic_coerce (Logic_const.tvar lth) Linteger in
  add_requires id (p_from_th th)

let add_ensures id p =
  let id_p = Logic_const.new_predicate p in
  Annotations.add_ensures Options.emitter (force_get id) [Normal, id_p]
    
let add_ensures_thread id p_from_th =
  let lth = Cil.cvar_to_lvar(th_parameter (force_get id)) in
  let th = Logic_const.tlogic_coerce (Logic_const.tvar lth) Linteger in
  add_ensures id (p_from_th th)
