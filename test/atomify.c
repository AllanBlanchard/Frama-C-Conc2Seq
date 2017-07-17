#include "../atomic.h"

void foo(int **p, int *a){
  int h[42];
  int x = 20;

  if(p[0][1] + 5 < a[2]){
    x = h[3];
  }
  
  p[a[3]][2] = 12;
  x = p[x][h[3]];
}
