#include "../../atomic.h"

#define SIZE 10

#define generate_globals(type, name)				\
  type  g##name ;						\
  type* gp##name ;						\
  type  ga##name[SIZE] ;					\
  type* gap##name[SIZE] ;					\
  type (*gpa##name) [SIZE] ;					\
  type  tl##name __attribute__((thread_local)) ;		\
  type* tlp##name __attribute__((thread_local)) ;		\
  type  tla##name[SIZE] __attribute__((thread_local)) ;		\
  type* tlap##name[SIZE] __attribute__((thread_local)) ;	\
  type (*tlpa##name) [SIZE] __attribute__((thread_local))

#define generate_locals(type)				\
  type  l ;						\
  type* lp ;						\
  type  la[SIZE] ;					\
  type* lap[SIZE] ;					\
  type (*lpa) [SIZE]

#define generate_formals(type)						\
  type f, type* fp, type fa[SIZE], type* fap[SIZE], type (*fpa) [SIZE]

#define assigns_local_formals			\
  l  = f ;					\
  lp = fp ;					\
  for(int i = 0 ; i < SIZE ; ++i){		\
    la[i]     = fa[i] ;				\
    lap[i]    = fap[i] ;			\
    *(lpa)[i] = *(fpa)[i] ;			\
  }
  
struct type {
  int  x;
  char y;
};

generate_globals(int, i);
generate_globals(char, c);
generate_globals(struct type, s);

void f_int(generate_formals(int)){
  generate_locals(int);
  assigns_local_formals ;
}

void f_char(generate_formals(char)){
  generate_locals(char);
  assigns_local_formals ;
}

void f_struct(generate_formals(struct type)){
  generate_locals(struct type);
  assigns_local_formals ;
}
