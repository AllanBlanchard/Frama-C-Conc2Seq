#include "../atomic.h"

#define MAX 42

need_compare_exchange(int);
need_fetch_and_sub(int);

int counter;
//@ ghost int th_counter __attribute__((thread_local));

/*@ logic integer sum(integer i, integer j) = i+j ; */

/*@ 
  global invariant is_sum:
    counter == (int) thread_reduction(sum, th_counter, 0) < 42;
*/

/*@ requires 0 <= a <= 10; */
int inc_by(int a){
  int x = counter;
  if(x + a < MAX){
    int r;
    /*@ atomic \true; */{
      r = compare_exchange(int, &a, x, x+a);
      //@ ghost th_counter += a;
    }
    if(r) return a;
    else  return -1;
  }
  return -1;
}

void dec(){
  /*@ atomic \true; */{
    fetch_and_sub(int, &counter, 1);
    //@ ghost th_counter--;
  }
}
