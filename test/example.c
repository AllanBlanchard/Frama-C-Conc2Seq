#include "../atomic.h"

#define MAX 30

int x;

/*@ global invariant Xvalue: 0 <= x < MAX; */

/*@ requires 0 < y < 60; */
void add(int y){
  if(x+y < MAX)
    x += y;
}

void decr(){
  if(x > 0)
    x--;
}
