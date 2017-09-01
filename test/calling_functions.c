#include "../atomic.h"

int global;
int thread_local __attribute__((thread_local));

/*@ 
  requires 0 < formal < global ; 
  ensures  thread_local < 10 ;
  ensures  \result < 1000 ;
*/
int bar(int formal){
  int local = formal+2 ;
  return thread_local+local;
}

void foo(int formal){
  global += formal ;
  int h = bar(formal) ;
  global += h ;

  /*
    identified bug :
    global += bar(formal) ;
  */
}

void foo_bar(){
  foo(global);
}
