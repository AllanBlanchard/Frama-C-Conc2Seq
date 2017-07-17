#include "../atomic.h"

/*@ atomic \true ; */
void foo(){

}

void bar(){
  /*@ atomic \true ; */{
    foo();
  }
}
