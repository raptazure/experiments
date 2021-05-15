#include <stdlib.h>

typedef struct node
{
  int procID;
  int releaseTime;
  int priority;
  int cpuTime;
  int executedTime;
  int state;
  struct node *next;
} PCB;