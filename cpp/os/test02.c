#include <pthread.h>
#include <stdio.h>
#define NUM_THREADS 5

int main(int argc, char *argv[])
{
  int i, scope;
  pthread_t tid[NUM_THREADS];
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  if (pthread_attr_getscope(&attr, &scope) != 0)
  {
    fprintf(stderr, "unable to get scheduing scope\n");
  }
  else
  {
    if (scope == PTHREAD_SCOPE_PROCESS)
    {
      printf("PTHREAD_SCOPE_PROCESS");
    }
    else if (scope == PTHREAD_SCOPE_SYSTEM)
    {
      printf("PTHREAD_SCOPE_SYSTEM");
    }
    else
    {
      fprintf(stderr, "illegal scope value.\n");
    }
  }
}
