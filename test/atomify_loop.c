#include "../atomic.h"

int global;

int simple_loop(int *a, int x){
  while(*a < global){
    x++;
  }
  return *a+x;
}

int double_cond(int *a, int x){
  int i = 0;
  while(i < *a && i < x){
    ++i;
  }
  return i;
}
