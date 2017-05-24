open Cil_types

module Vmap = Map.Make(struct type t = int let compare = compare end)

let create_simulation base var =
  let name = base ^ "_" ^ var.vname in
  let simulation = Cil.makeGlobalVar name (TPtr (var.vtype, [])) in
  Globals.Vars.add_decl simulation ;
  simulation

let globals  = ref Vmap.empty
let thlocals = ref Vmap.empty
let locals   = ref Vmap.empty
let fromvars = ref Vmap.empty
let pc       = ref None

let initialize_pc () =
  let rpc = Cil.makeGlobalVar "pc" (TPtr (Cil.intType, [])) in
  Globals.Vars.add_decl rpc ;
  pc := Some rpc
                   
let add_global var init =
  assert(var.vglob) ;
  if Thread_local.is_thread_local var then
    let simulation = create_simulation "tl" var in    
    thlocals := Vmap.add var.vid simulation !thlocals
  else 
    globals := Vmap.add var.vid (var, init) !globals

let add_local func var =
  assert (not var.vglob) ;
  let fname = (Globals.Functions.get_vi func).vname in
  let simulation = create_simulation fname var in
  locals  := Vmap.add var.vid simulation !locals

let add_function func =
  let vi = Globals.Functions.get_vi func in
  let name = "from_" ^ vi.vname in
  let simulation = Cil.makeGlobalVar name (TPtr (Cil.intType, [])) in
  Globals.Vars.add_decl simulation ;
  fromvars := Vmap.add vi.vid simulation !fromvars

let get_pc () = match !pc with None -> assert false | Some v -> v
                      
let simulations loc =
  let p = get_pc() in
  let g = Vmap.fold (fun _ (v, _) l -> v :: l) !globals [] in
  let t = Vmap.fold (fun _ v l      -> v :: l) !thlocals [] in
  let l = Vmap.fold (fun _ v l      -> v :: l) !locals [] in
  let f = Vmap.fold (fun _ v l      -> v :: l) !fromvars [] in
  let all = p :: g @ t @ l @ f in
  List.map (fun v -> GVar(v, {init=None}, loc)) all


let access vid ?th:(th=None) ?no:(no=NoOffset) loc =
  match th with
  | None ->
     assert(Vmap.mem vid !globals) ;
     Var( fst (Vmap.find vid !globals) ), no
  | Some th ->
     let ptr = if      vid = -1               then get_pc ()
               else if Vmap.mem vid !locals   then Vmap.find vid !locals
               else if Vmap.mem vid !thlocals then Vmap.find vid !thlocals
               else if Vmap.mem vid !fromvars then Vmap.find vid !fromvars
               else assert false
     in
     let exp = Cil.mkBinOp ~loc:loc PlusPI (Cil.evar ptr) (Cil.evar th) in
     Cil.mkMem ~addr:exp ~off:no
