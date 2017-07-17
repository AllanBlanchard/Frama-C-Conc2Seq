#include "../atomic.h"

void foo(int *a){
  for(int i = 0; i < 10; ++i){
    if(i%2) continue;
    *a += i;
  }
}
