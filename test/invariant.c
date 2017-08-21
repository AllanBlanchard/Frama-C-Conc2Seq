#include "atomic.h"

int global = 1;
int th_local __attribute__((thread_local)) = 0 ;


/*@
  predicate nested{L}(integer x) =
    th_local < x ;
*/

/*@
  global invariant direct_definition: global > th_local ;
  global invariant nested_definition: nested(42);
  global invariant both: nested(34) && th_local % 2 == 0 ;
*/


/* Currently not parsed by Frama-C
void foo(){
  static int x = 0;
  //@ global invariant test2: x >= 0; 
}
*/


void bar(int a){
  a += 2;
}
