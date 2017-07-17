#include "../atomic.h"

need_compare_exchange(int);
need_fetch_and_sub(int);
need_atomic_store(int);

int data;
int access;
//@ghost int reader __attribute__((thread_local));
//@ghost int writer __attribute__((thread_local));

/*@ logic integer sum(integer a, integer b) = a+b ; */

/*@
  predicate pswtw =
    access == -1 <==>     (1      ==       thread_reduction(sum, writer, 0));
  global invariant single_writer_to_writer: pswtw ;
  
  predicate pswtr =
    access == -1 <==>     (0      ==       thread_reduction(sum, reader, 0));
  global invariant single_writer_to_reader: pswtr ;

  predicate pmrtr =
    access >=  0 <==>     (access == (int) thread_reduction(sum, reader, 0));
  global invariant multiple_readers_to_reader: pmrtr ;
  
  predicate pmrtw =
    access >=  0 <==>     (0      ==       thread_reduction(sum, writer, 0));
  global invariant multiple_readers_to_writer: pmrtw ;

  global invariant sw_mr:
    access >= -1;

  global invariant ghost_values:
    0 <= writer <= 1 && 0 <= reader <= 1;

  predicate pct_imply_for_thread = \true;

  global invariant pct_implication:
    pct_imply_for_thread;
*/

int write(int value){
  int r;
  int exp = 0;
  /*@ atomic \true; */{
    r = compare_exchange(int, &access, &exp, -1);
    //@ ghost writer = (r == 1) ;      
  }
  if(! r ) return 0;

  data = value;

  /*@ atomic \true; */{
    atomic_store(int, &access, 0);
    //@ ghost writer = 0;
  }
  return 1;
}

/*@
  requires \valid(loc) && \separated(loc, &access, &data);
*/
int read(int* loc){
  int r;
  int a = access;
  if(a >= 0){
    /*@ atomic \true; */{
      r = compare_exchange(int, &access, &a, a+1);
      //@ ghost reader = (r == 1) ;      
    } 
  }
  else     return 0;
  if(! r ) return 0;

  *loc = data;

  /*@ atomic \true; */{
    fetch_and_sub(int, &access, 1);
    //@ ghost reader = 0;
  }
  return 1;
}
