#include "../atomic.h"

void bar(){
  int a = 42;
  /*@ atomic \true; */{
    int x = a;
    /*@ atomic \true; */{
      int j = x;
      x += j += a;
    }
  }
}
