open Cil_types
(*open Logic_const*)
open Logic_typing


let count = ref 0 
let atomic_typer ~typing_context ~loc ps =
  let open Logic_ptree in
  match ps with
  | p :: [] when p.lexpr_node = PLtrue ->
     Ext_preds [ typing_context.type_predicate
                   typing_context
                   (typing_context.post_state [Normal])
                   p
               ]
  | [] ->
     let id = !count in incr count ; Ext_id id
  | _ ->
     typing_context.error loc "expecting a \true after keyword atomic"

(* Definition of atomic extension for ACSL *)
(* check if the given behavior bhv contains "atomic" mention *)
let is_atomic_behavior bhv = 
  let atomic b = match b with ("atomic", Ext_preds _) -> true | _ -> false in
  List.exists atomic bhv.b_extended

(* Check if a specification contains "atomic" mention *)
let contains_atomic spec =
  List.exists is_atomic_behavior spec.spec_behavior

(* Check if the given statement is tagged atomic *)
let atomic_stmt s =
  let annots = Annotations.code_annot s in
  let atomic annot = match annot.annot_content with
    | AStmtSpec(_, spec) -> contains_atomic spec
    | _ -> false
  in
  List.exists atomic annots

let atomic_fct kf =
  contains_atomic (Annotations.funspec kf)

let atomic_call instr =
  let fct = match instr with
    | Call(_, e, _, _) ->
       begin match e.enode with
       | Lval(Var(fct), NoOffset) -> fct
       | _ -> raise (Errors.BadConstruct "Function pointers")
       end
    | Local_init(_, ConsInit(fct, _, _), _) -> fct
    | _ -> assert false
  in
  let kf = Globals.Functions.get fct in
  atomic_fct kf

let atomic_call_stmt s =
  match s.skind with
  | Instr(i) -> atomic_call i
  | _ -> assert false
             
let () = register_behavior_extension "atomic" atomic_typer
