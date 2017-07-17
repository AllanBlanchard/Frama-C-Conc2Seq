typedef unsigned uint;

#include "../atomic.h"
#include "occ_axiom.h"

#define NUMBER_OF_PAGES 4096
#define PAGE_SIZE       1024
#define MAX_MAPPINGS     256

need_atomic_load(uint);
need_compare_exchange(uint);
need_atomic_exchange(uint);
need_fetch_and_sub(uint);

enum page_type{
  ZERO,
  DATA,
  TABLE
};

typedef uint* page_t;

struct page_desc{
  enum page_type pt;
  uint mappings;
};

struct page_desc descriptors[NUMBER_OF_PAGES];
uint memory[NUMBER_OF_PAGES*PAGE_SIZE];

/*@
  logic page_t frame(integer j) =
    memory + j*PAGE_SIZE;

  lemma separated_frames:
    \forall integer j,k; j != k ==> 
      \separated(frame(j)+(0 .. PAGE_SIZE-1), frame(k)+(0 .. PAGE_SIZE-1));
*/

/*@
  requires 0 <= fn < NUMBER_OF_PAGES;
  assigns  \nothing;
  ensures  \valid( \result + (0 .. PAGE_SIZE-1) );
  ensures  &memory[0] <= \result < &memory[(NUMBER_OF_PAGES-1) * PAGE_SIZE] ;
  ensures  \result == frame(fn);
  atomic \true;
*/
static inline page_t get_frame(uint fn){
  return &memory[fn * PAGE_SIZE];
}

/*@
  predicate same_type{L1,L2}(struct page_desc* pd) = \at( (*pd).pt, L1) == \at( (*pd).pt, L2);

  predicate bounded_pagetable(integer j) =
    \forall integer k; 0 <= k < PAGE_SIZE ==> 0 <= frame(j)[k] < NUMBER_OF_PAGES;
*/
/*@
  logic integer l_mappings{L}(integer e, integer fn) =
    (descriptors[fn].pt == TABLE) ? occ_a{L}(e, frame(fn), 0, PAGE_SIZE) : 0;
*/
/*@
  axiomatic Mappings{
    logic integer mappings{L}(integer e, integer from, integer to) 
      reads *(memory+(from.. (to-1) * PAGE_SIZE-1)), *(descriptors+(from .. to - 1));
  
    axiom end_occ_m{L}:
      \forall integer e, integer from, to;
        from >= to ==> mappings{L}(e, from, to) == 0;
	
    axiom iter_occ_m{L}:
      \forall integer e, integer from, to;
        from < to ==>
	  mappings{L}(e,from,to) == l_mappings{L}(e, to-1) + mappings{L}(e,from,to-1);
  }

  lemma mappings_separable{L}:
    \forall integer e, from, cut, to; from <= cut <= to ==>
      mappings{L}(e, from, to) == mappings{L}(e, from, cut) + mappings{L}(e, cut, to);

  predicate same_memory{L1,L2}(integer from, integer to) =
    \forall integer j; from <= j < to ==> same_type{L1,L2}(\at(descriptors+j, L1)) &&
    \forall integer j; from <= j < to ==> same_elems{L1,L2}(\at(frame(j),L1), 0, PAGE_SIZE);
      
  lemma same_memory_same_mappings{L1, L2}:
    \forall integer e, integer from, to;
      same_memory{L1,L2}(from, to) ==> mappings{L1}(e, from, to) == mappings{L2}(e, from, to);

  predicate one_entry_change{L1, L2}(integer fn, integer idx, integer from, integer to) =
    from <= fn < to && same_memory{L1,L2}(from, fn) && same_memory{L1,L2}(fn+1, to) &&
    \at(descriptors[fn].pt, L1) == TABLE && one_change{L1,L2}(idx, \at(frame(fn),L1), 0, PAGE_SIZE);

  lemma one_entry_change_means_inc_and_dec_mappings{L1, L2}:
    \forall integer fn, idx, integer from, to;
      \let pv_idx = \at(frame(fn)[idx],L1); \let cv_idx = \at(frame(fn)[idx],L2);
      (one_entry_change{L1,L2}(fn, idx, from, to)) ==> (
        mappings{L1}(pv_idx, from, to)-1 == mappings{L2}(pv_idx, from, to) &&
	mappings{L1}(cv_idx, from, to)+1 == mappings{L2}(cv_idx, from, to)
      );

  lemma one_entry_change_means_all_other_equal{L1, L2}:
    \forall integer fn, idx, integer from, to, other;
      (one_entry_change{L1,L2}(fn, idx, from, to) && 
      other != \at(frame(fn)[idx],L1) && 
      other != \at(frame(fn)[idx],L2)) ==>
        mappings{L1}(other, from, to) == mappings{L2}(other, from, to);   

  lemma mappings_bound{L}:
    \forall integer e, integer from, to;
      from <= to ==> 0 <= mappings(e,from,to);
*/

//@ ghost uint gap __attribute__((thread_local)) ;

/*@
  global invariant memory_invariant:
    \forall integer j; 0 <= j < NUMBER_OF_PAGES ==>
      0 <= mappings(j, 0, NUMBER_OF_PAGES) <= descriptors[j].mappings <= MAX_MAPPINGS;

  global invariant gap_invariant:
    0 <= gap < NUMBER_OF_PAGES ;
*/

/*@
  requires 0 <= entry < PAGE_SIZE;
  requires 0 <= fn <  NUMBER_OF_PAGES;
  requires 0 <= new < NUMBER_OF_PAGES;
  requires descriptors[fn].pt == TABLE;

  assigns memory[fn*PAGE_SIZE + entry], descriptors[new].mappings;
*/
int exchange(uint fn, uint entry, uint new){
  //@ ghost gap = 0;
  uint* mappings = &descriptors[new].mappings;
  
  uint c_n = atomic_load(uint, mappings);
  if(c_n >= MAX_MAPPINGS) return ~0;

  int cas_mappings;
  /*@ atomic \true ; */{
    cas_mappings = compare_exchange(uint, mappings, &c_n, c_n+1) ;
    /*@ ghost gap = (cas_mappings) ? new : 0 ;*/
  }

  if(!cas_mappings) return ~0;

  uint old;
  /*@ atomic \true ; */{
    old = atomic_exchange(uint, & (get_frame(fn)[entry]), new);
    /*@ ghost gap = old ; */
  }

  if(old)/*@ atomic \true ; */{
    fetch_and_sub(uint, &descriptors[old].mappings, 1);
    /*@ ghost gap = 0 ; */
  }
  return 0;
}
