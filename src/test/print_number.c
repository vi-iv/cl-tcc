#include <stdio.h>

extern int test();

int main(void)
{
  printf("RESULT: %d\n", test());\
  return 0;
}
