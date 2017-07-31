open Cil_types
open Logic_const


let make_axiom loc id =
  let j   = Cil_const.make_logic_var_quant "j" Linteger in
  let lj  = tvar j in
  let lbl = LogicLabel (None, "L") in
  let valid_th = Atomic_header.valid_thread_id lbl lj in
  let mem_loc  = pvalid ~loc (lbl, Vars.l_memloc id lj loc) in
  let tmp  = pforall([j], pimplies(valid_th, mem_loc)) in
  let impl = pimplies((Simulation_invariant.app loc lbl), tmp) in
  let name = (Vars.sname id) ^ "_is_valid" in
  Dlemma(name, true, [lbl], [], impl, [], loc)
    
let make_range lbl loc id =
  let max_th = Atomic_header.max_thread lbl in
  let up_bound = term (TBinOp (MinusA, max_th, tinteger 1)) Linteger in
  let range = trange ~loc (Some (tinteger 0), Some up_bound) in
  Vars.l_memloc id range loc

let make_ranges lbl loc = 
  List.map (make_range lbl loc) (Vars.ids())

let gvar_range loc vi =
  let open Logic_const in
  let open Logic_utils in
  match vi.vtype with
  | TInt(_) | TFloat(_) | TNamed(_) | TComp(_) | TEnum(_) ->
    let exp = Cil.new_exp ~loc (AddrOf(Var(vi), NoOffset)) in
    expr_to_term ~cast:false exp
  | TArray(typ, Some(size),_,_) ->
    let lsize = expr_to_term ~cast:true size in
    let max   = term (TBinOp(MinusA, lsize, (tinteger 1))) lsize.term_type in
    let range = trange ~loc ((Some (tinteger 0)), (Some max)) in
    let lv = Cil.cvar_to_lvar vi in
    let tlval = TVar(lv), TIndex(range, TNoOffset) in
    mk_logic_AddrOf ~loc tlval (Ctype (TPtr(typ,[])))
  | _ -> assert false
    
let gvars_ranges loc =
  let (usable, unusable) = List.partition
      (fun vi ->
         match vi.vtype with
         | TArray(_,None,_,_) | TPtr(_) | TVoid(_) | TFun(_)
         | TBuiltin_va_list(_) -> false
         | _ -> true
      )(Vars.global_vis())
  in
  List.iter(fun vi ->
      match vi.vtype with
      | TPtr(_) ->
        Options.Self.warning
          "%a is a pointer, separation with simulation is not currently supported"
          Cil_datatype.Varinfo.pretty vi
      | _ -> assert false
    ) unusable ;
  List.map (gvar_range loc) usable

let make_separation loc =
  let lbl = LogicLabel (None, "L") in
  let sim_ranges = make_ranges lbl loc in
  let glo_ranges = gvars_ranges loc in
  let p = pseparated ~loc (glo_ranges @ sim_ranges) in
  let impl = pimplies((Simulation_invariant.app loc lbl), p) in
  let name = "all_separated" in
  Dlemma(name, true, [lbl], [], impl, [], loc)

let add_vars_to_simulation_inv loc =
  let vars = List.map (fun i -> Vars.l_ptrvalue i loc) (Vars.ids ()) in
  Simulation_invariant.reads vars
    
let get loc =
  let vars_axioms = List.map (make_axiom loc) (Vars.ids()) in
  let sepa = make_separation loc in
  add_vars_to_simulation_inv loc ;
  sepa :: vars_axioms
    
