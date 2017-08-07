open Cil_types

let lemmas = ref []

let register name labels predicate =
  let th = Cil_const.make_logic_var_quant "th" Linteger in
  let loc = Cil.CurrentLoc.get() in
  let visitor = Fun_preds.make_visitor (Logic_const.tvar th) loc in
  let p = Visitor.visitFramacPredicate visitor predicate in
  let predicate = Logic_const.pforall ([th], p) in
  lemmas := (name, labels, predicate) :: !lemmas

  
let globals loc =
  let globals = List.map (
      fun (name, lbls, p) ->
        let lemma = Dlemma(name, false, lbls, [], p, [], loc) in
        Annotations.add_global Options.emitter lemma ;
        GAnnot(lemma, loc)
    ) !lemmas
  in
  List.rev globals
