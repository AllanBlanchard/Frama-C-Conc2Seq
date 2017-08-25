#include "../atomic.h"

int global;
int thread_local __attribute__((thread_local));

int bar(int formal){
  int local = formal+2 ;
  return thread_local+local;
}

void foo(int formal){
  global += formal ;
  global ++ ;
}

void foo_bar(){
  foo(global);
}
