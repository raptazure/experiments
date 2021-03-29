#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>

int main()
{
  pid_t pid;
  /* fork another process */
  pid = fork();
  if (pid < 0)
  { /* error occurred */
    fprintf(stderr, "Fork Failed");
    exit(-1);
  }
  else if (pid == 0)
  { /* child process */
    printf("I am a Child\n");
  }
  else
  { /* parent process */
    /* parent will wait for the child to complete */
    wait(NULL);
    printf("I am a Parent\n");
    exit(0);
  }
}
