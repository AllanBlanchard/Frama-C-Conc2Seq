//Axiomes et lemmes pour le comptage sur une page

/*@
  axiomatic OccArray{
    logic integer occ_a{L}(integer e, uint* t, integer from, integer to) reads *(t+(from .. to - 1));
  
    //negative range
    axiom end_occ_a{L}:
      \forall integer e, uint* t, integer from, to;
        from >= to ==> occ_a{L}(e,t, from, to) == 0;
	
    //positive range + element OK
    axiom iter_occ_a_true{L}:
      \forall integer e, uint* t, integer from, to;
        (from < to && t[to-1] == e) ==> occ_a{L}(e,t,from,to) == occ_a{L}(e,t,from,to-1) + 1;

    //positive range + element KO
    axiom iter_occ_a_false{L}:
      \forall integer e, uint* t, integer from, to;
        (from < to && t[to-1] != e) ==> occ_a{L}(e,t,from,to) == occ_a{L}(e,t,from,to-1);
  }
*/

/*@
  //we can cut the array and add the results
  lemma occ_a_separable{L}:
    \forall integer e, uint* t, integer from, cut, to;
      from <= cut <= to ==> occ_a{L}(e,t,from,to) == occ_a{L}(e,t,from,cut)+occ_a{L}(e,t,cut,to);
*/

/*@
  //no element changed
  predicate same_elems{L1,L2}(uint *t, integer from, integer to) =
    \forall integer j; from <= j < to ==> \at(t[j], L1) == \at(t[j], L2);

  //same number of occ_aurrences
  lemma same_elems_means_same_occ_a{L1, L2}:
    \forall integer e, uint* t, integer from, to;
      same_elems{L1,L2}(t,from,to) ==> occ_a{L1}(e, t, from, to) == occ_a{L2}(e, t, from, to);
*/

/*@
  //one element changed
  predicate one_change{L1,L2}(integer index, uint *t, integer from, integer to) =
    from <= index < to &&
    same_elems{L1,L2}(t, from, index) && same_elems{L1,L2}(t, index+1, to) &&
    \at(t[index], L1) != \at(t[index], L2);

  //new element occ_aurrence inc, old dec and other didn't change
  lemma one_change_means_inc_and_dec{L1, L2}:
    \forall integer index, uint* t, integer from, to;
      \let pv_idx = \at(t[index],L1); \let cv_idx = \at(t[index],L2);
      (one_change{L1,L2}(index, t, from, to)) ==> (
        occ_a{L1}(pv_idx, t, from, to)-1 == occ_a{L2}(pv_idx, t, from, to) &&
	occ_a{L1}(cv_idx, t, from, to)+1 == occ_a{L2}(cv_idx, t, from, to) &&
	(\forall integer other; other != pv_idx && other != cv_idx ==> 
	   occ_a{L1}(other, t, from, to) == occ_a{L2}(other, t, from, to)
	)
      );
*/

//This ones are not necessary at the moment (but are proven)
/*@
  lemma occ_a_bound{L}:
    \forall integer e, uint* t, integer from, to;
      from <= to ==> 0 <= occ_a(e,t,from,to) <= to-from;
*/
/* @
  predicate not_in{L}(integer e, uint *t, integer from, integer to) =
    \forall integer j; from <= j < to ==> t[j] != e;

  lemma not_in_means_0_occ_a{L}:
    \forall integer e, uint* t, integer from, to;
      not_in{L}(e, t, from, to) ==> occ_a{L}(e, t, from, to) == 0;
*/
