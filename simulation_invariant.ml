open Cil_types

let invariant = ref None

let build () =
  match !invariant with
  | None ->
    let i = { (Cil_const.make_logic_info "simulation") 
              with l_labels = [FormalLabel("L")] }
    in
    Logic_utils.add_logic_function i ;
    invariant := Some i ;
    i
  | Some i -> i

let gannot = ref None

let get loc =
  match !gannot with
  | None ->
    let li = build () in
    let g  = Dfun_or_pred(li, loc) in
    Annotations.add_global Options.emitter g ;
    gannot := Some g ;
    g
  | Some g -> g

let app loc lbl =
  let li = build () in
  Logic_const.papp ~loc (li, [lbl], [])

let reads l =
  let li = build () in
  li.l_body <- LBreads (List.map Logic_const.new_identified_term l)
