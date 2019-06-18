#ifndef UTIL_H
#define UTIL_H

#define ABORT(x) do { \
  fprintf(stderr, "aborted at %s:%i: %s\n", __FILE__, __LINE__, x); \
  exit(1); } while(0)

#define ASSERT(b, err) do { \
 if (!(b)) \
  { ABORT("assert_failed: " err); \
  }; \
 } while(0)

#endif
