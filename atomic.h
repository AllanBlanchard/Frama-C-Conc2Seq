#include <stddef.h>

// To transform this into a builtin and communicate directly with WP,
// have a look to the WP manual, particularly on the drivers part

/*@
  axiomatic thread_variables_properties{
  logic integer MAX_THREAD reads \nothing;

  axiom some_threads: 0 < MAX_THREAD;
  }

  predicate valid_thread_id(integer th) =
    0 <= th < MAX_THREAD;
*/

//@ ghost size_t world;

/*@
  assigns world;
  ensures valid_thread_id(\result);
  atomic \true ;
*/
size_t some_thread();


#define need_atomic_load(TYPE)						\
  /*@									\
    requires \valid(loc);						\
    assigns  \nothing;							\
    ensures  \result == *loc;						\
    atomic   \true;							\
  */									\
  TYPE atomic_load_##TYPE(TYPE volatile* loc)

#define atomic_load(TYPE, loc) atomic_load_##TYPE(loc)

#define need_atomic_store(TYPE)						\
  /*@									\
    requires \valid(loc);						\
    assigns  *loc;							\
    ensures  *loc == des;						\
    atomic   \true;							\
  */									\
  void atomic_store_##TYPE(TYPE volatile* loc, TYPE des)

#define atomic_store(TYPE, loc, des) atomic_store_##TYPE(loc, des)


#define need_atomic_compare_exchange(TYPE)				\
  /*@									\
    requires \valid(loc) && \valid(exp);				\
    assigns  *loc, *exp;						\
    atomic   \true ;							\
    ensures  \result == 1 || \result == 0;				\
    behavior succeed:                                                   \
      assumes *exp == *loc;						\
      ensures \result == 1;						\
      ensures *loc == des ;                                             \
      ensures *exp == \old(*exp) ;                                      \
    behavior failed:                                                    \
      assumes *exp != *loc;						\
      ensures \result == 0;                                             \
      ensures *loc == \old(*loc) ;                                      \
      ensures *exp == \old(*loc) ;                                      \
    disjoint behaviors ;                                                \
    complete behaviors ;                                                \
  */									\
  int atomic_compare_exchange_strong_##TYPE(TYPE volatile* loc, TYPE* exp, TYPE des)

#define atomic_compare_exchange(TYPE, loc, exp, des) atomic_compare_exchange_strong_##TYPE(loc, exp, des)

#define need_atomic_exchange(TYPE)					\
  /*@									\
    requires \valid(loc);						\
    assigns  *loc;							\
    ensures  \result == \old(*loc);					\
    ensures  *loc == des;						\
    atomic   \true;							\
  */									\
  TYPE atomic_exchange_strong_##TYPE(TYPE volatile* loc, TYPE des)

#define atomic_exchange(TYPE, loc, des)	atomic_exchange_strong_##TYPE(loc, des)

#define need_atomic_fetch_add(TYPE)					\
  /*@									\
    requires \valid(loc);						\
    assigns  *loc;							\
    ensures  *loc == (int) \result + val;				\
    ensures  \result == \old(*loc);					\
    atomic   \true;							\
  */									\
  TYPE atomic_fetch_add_##TYPE(TYPE volatile* loc, TYPE val)

#define atomic_fetch_add(TYPE, loc, val) atomic_fetch_add_##TYPE(loc, val)


#define need_atomic_fetch_sub(TYPE)					\
  /*@									\
    requires \valid(loc);						\
    assigns  *loc;							\
    ensures  *loc == (int) \result - val;				\
    ensures  \result == \old(*loc);					\
    atomic   \true;							\
  */									\
  TYPE atomic_fetch_sub_##TYPE(TYPE volatile* loc, TYPE val)

#define atomic_fetch_sub(TYPE, loc, val) atomic_fetch_sub_##TYPE(loc, val)

#define need_atomic_fetch_or(TYPE)					\
  /*@									\
    requires \valid(loc);						\
    assigns  *loc;							\
    ensures  *loc == ((int) \result | val);				\
    ensures  \result == \old(*loc);					\
    atomic   \true;							\
  */									\
  TYPE atomic_fetch_or_##TYPE(TYPE volatile* loc, TYPE val)

#define atomic_fetch_or(TYPE, loc, val) atomic_fetch_or_##TYPE(loc, val)

#define need_atomic_fetch_xor(TYPE)					\
  /*@									\
    requires \valid(loc);						\
    assigns  *loc;							\
    ensures  *loc == ((int) \result ^ val);				\
    ensures  \result == \old(*loc);					\
    atomic   \true;							\
  */									\
  TYPE atomic_fetch_xor_##TYPE(TYPE volatile* loc, TYPE val)

#define atomic_fetch_xor(TYPE, loc, val) atomic_fetch_xor_##TYPE(loc, val)

#define need_atomic_fetch_and(TYPE)					\
  /*@									\
    requires \valid(loc);						\
    assigns  *loc;							\
    ensures  *loc == ((int) \result & val);				\
    ensures  \result == \old(*loc);					\
    atomic   \true;							\
  */									\
  TYPE atomic_fetch_and_##TYPE(TYPE volatile* loc, TYPE val)

#define atomic_fetch_and(TYPE, loc, val) atomic_fetch_and_##TYPE(loc, val)

#define ATOMIC(x) /*@ atomic \true; */{ x }
