#include "../atomic.h"

int foo(int a){
  switch(a){
  case 0 : a += 3; 
  case 1 : a += 5; break;
  case 2 : a += 6; a += a; break;
  case 3 : a += 8; break;
  default: a -= 2; break;
  }
  return a;
}
