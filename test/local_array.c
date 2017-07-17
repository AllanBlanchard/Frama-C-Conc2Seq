#include "../atomic.h"

int foo(int i){
  int p[10];
  for(int j = 0 ; j < 10; ++j)
    p[j] = i;
}
