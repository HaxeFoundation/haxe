#if (!defined(ALLOC_PRF) || !defined(ALLOC_TYPE))
#error
#endif

#include "util.h"
#include <stdlib.h>
#include <stdio.h>


ALLOC_TYPE* ALLOC_PRF(_alloc)(size_t count)
 {
 return malloc(count * sizeof(ALLOC_TYPE));
 }

void ALLOC_PRF(_free)(ALLOC_TYPE* arr)
 {
 free(arr);
 }

ALLOC_TYPE* ALLOC_PRF(_realloc)(ALLOC_TYPE* arr, size_t count)
 {
 size_t newsz = count * sizeof(ALLOC_TYPE);
 ALLOC_TYPE* newarr = realloc(arr, newsz);
 if (count != 0 && newarr == NULL)
  {
  static char msg[128];
  sprintf
    ( msg
    , "realloc(%p, %u*%u=%u) failed: to provide an alternative behaviour."
    , arr, (unsigned int) count, (unsigned int) sizeof(ALLOC_TYPE)
    , (unsigned int) newsz
    );
  ABORT(msg);
  };
 return newarr;
 }


#undef ALLOC_PRF
#undef ALLOC_TYPE
