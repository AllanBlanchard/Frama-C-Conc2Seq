#include "../atomic.h"

int a[42];

int foo(int v, int* p){
  int t[4];
  int h = a[40];

  if(v + p[0] + t[1] + a[5] < 15 && v == 0){
    a[3] = 22;
  } else {
    a[41] = 25 + p[0];
    return a[41];
  }
  return a[3];
}

int main(){
  
}
