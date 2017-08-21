#include "atomic.h"

int global;
int th_local __attribute__((thread_local));

/*@
  predicate only_involves_global{L} =
    global > 0;

  logic integer involve_thread_local{L} =
    th_local * global ;

  predicate transitively_involve_thread_local{L}(int a) =
    a < involve_thread_local{L} ;
*/

/*@
  lemma no_thread_local{L}:
    \forall integer n; 0 < global < n;

  lemma direct_thread_local{L}:
    \forall integer n; 0 < th_local < n;

  lemma hidden_thread_local{L}:
    \forall int n; n < global && transitively_involve_thread_local(n);
*/
