open Cil_types

let lemmas = ref []

let register name labels predicate =
  let open Logic_const in
  let th = Cil_const.make_logic_var_quant "th" Linteger in
  let loc = Cil.CurrentLoc.get() in
  let visitor = Fun_preds.make_visitor (tvar th) loc in
  let p = Visitor.visitFramacPredicate visitor predicate in
  let valid_th = Atomic_header.valid_thread_id (tvar th) in
  let p = pforall ([th], pimplies (valid_th , p)) in
  lemmas := (name, labels, p) :: !lemmas

  
let globals loc =
  let globals = List.map (
      fun (name, lbls, p) ->
        let lemma = Dlemma(name, false, lbls, [], p, [], loc) in
        Annotations.add_global Options.emitter lemma ;
        GAnnot(lemma, loc)
    ) !lemmas
  in
  List.rev globals
