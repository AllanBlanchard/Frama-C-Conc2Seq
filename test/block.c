#include "../atomic.h"

int foo(int* a, int b){
  b += *a;
  {
    int c;
    c = b;
    *a = c;
  }
  return b;
}
