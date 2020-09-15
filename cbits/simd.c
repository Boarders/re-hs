#include <emmintrin.h>
#include <stdint.h>
#include <stdio.h>

const uint8_t a[16]
  = {1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4};

uint8_t b[16] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};


int simd_comp(__m128i p_val, uint8_t * t);

/*
void main() {
  __m128i mm_a = _mm_loadu_si128(a);  
  printf("%d\n", simd_comp(mm_a,b));
}
*/

int simd_comp(__m128i p_val, uint8_t* t){
  //__m128i mm_p = _mm_loadu_si128(p);
  __m128i mm_t = _mm_loadu_si128(t);
  return _mm_movemask_epi8(_mm_cmpeq_epi8(p_val, mm_t));
}
