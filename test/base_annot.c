#include "../atomic.h"

void foo(int a, int* p){
  //@assert \valid(p+a);
  p[a] = 42;
  //@assert p[a] == 42;
}
