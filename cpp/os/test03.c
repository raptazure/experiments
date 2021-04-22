#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static void charatatime(char *str)
{
  char *ptr;
  int c;
  setbuf(stdout, NULL); /* set unbuffered */
  for (ptr = str; c = *ptr++;)
    putc(c, stdout);
}

int main(void)
{
  pid_t pid;
  if ((pid = fork()) < 0)
    perror("fork error");
  else if (pid == 0)
    charatatime("output from child\n");
  else
    charatatime("output from parent\n");
  exit(0);
}
