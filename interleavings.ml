open Cil_types

let choose_fun = ref None
let interleave = ref None

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
  [ { call with labels = [Case(n, loc)] } ; mkStmt (Break loc) ]

let case_choose vi th loc = case_identified 0 vi th loc

let case_stmt sid th loc =
  case_identified sid (Statements.simulation sid).svar th loc

let case_init sid th loc =
  case_identified (-sid) (Functions.simulation sid) th loc

let all_case_init th loc =
  List.flatten
    (List.map (fun s -> case_init s th loc) ( Functions.ids() ))

let all_case_stmt th loc =
  List.flatten
    (List.map (fun s -> case_stmt s th loc) (Statements.simulations ()))

let switch_stmt choose th loc = 
  let ch_call = case_choose choose th loc in
  let f_calls = all_case_init th loc in
  let s_calls = all_case_stmt th loc in
  let switch_b = Cil.mkBlock (ch_call @ f_calls @ s_calls) in
  let access = Vars.c_access (-1) ~th:(Some th) loc in
  let pct_th = Cil.new_exp ~loc:loc (Lval (access)) in
  Cil.mkStmt( Switch(pct_th, switch_b, switch_b.bstmts, loc))

let random_thread_stmt th loc =
  let lv_th = Some( (Var th), NoOffset ) in
  let fun_vi = Kernel_function.get_vi (Atomic_header.some_thread ()) in
  Cil.mkStmt (Instr (Call(lv_th, (Cil.evar fun_vi), [], loc)))

let build_code loc = 
  let open Cil in
  Options.Self.feedback "Generating interleaving function";
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
  interleave := Some def

let get_function loc =
  build_code loc ;
  match !interleave with
  | None -> assert false
  | Some fd -> GFun(fd, loc)

let get_choose loc =
  match !choose_fun with
  | None -> assert false
  | Some (fs, vi) -> GFunDecl(fs, vi, loc)
