#if ((!defined(PRF)))
#error
#endif

#define BITS_OF_CHAR 8

/*
size_t PRF(_sizebytes)(size_t n);
TYPE PRF(_get)(TYPE arr[], size_t i);
void PRF(_set)(TYPE arr[], size_t i, TYPE val);
*/

#define ALPRF(x) bitarrayalloc##x

#define ALLOC_TYPE unsigned char
#define ALLOC_PRF(x) ALPRF(x)
#include "alloc.c"


size_t wordalign(size_t n)
 {
 size_t al = sizeof(int);
 size_t m = al % n;
 if (m == 0)
  {
  return n;
  }
 else
  {
  return n + al - m;
  }
 }


size_t PRF(_sizebytes)(size_t n)
 {
 return wordalign(n/BITS_OF_CHAR);
 }


unsigned char* PRF(_alloc)(size_t count)
 {
 return ALPRF(_alloc)(PRF(_sizebytes)(count));
 }


void PRF(_free)(unsigned char* arr)
 {
 ALPRF(_free)(arr);
 }


unsigned char* PRF(_realloc)(unsigned char* arr, size_t newcount)
 {
 return ALPRF(_realloc)(arr, PRF(_sizebytes)(newcount));
 }


#define LVAL(arr, i) ((arr)[(i)/BITS_OF_CHAR])
#define MASK(i) (1<<((i)%BITS_OF_CHAR))

int PRF(_get)(unsigned char arr[], size_t i)
 {
 return ((LVAL(arr,i) & MASK(i)) ? 1 : 0);
 }

void PRF(_set)(unsigned char arr[], size_t i, int val)
 {
 unsigned char mask = MASK(i);
 if (val)
  {
  LVAL(arr,i) |= mask;
  }
 else
  {
  LVAL(arr,i) &= ~mask;
  }
 return;
 }

void PRF(_init)(unsigned char arr[], size_t sz, int val)
 {
 size_t bytesize = sz/BITS_OF_CHAR;
 size_t i;
 unsigned char valbyte = val ? (-1) : 0;
 for (i=0; i<bytesize; ++i)
  {
  arr[i] = valbyte;
  };
 i *= BITS_OF_CHAR;
 while(i < sz)
  {
  PRF(_set)(arr, i, val);
  ++i;
  };
 return;
 }


#undef MASK
#undef LVAL

#undef PRF
