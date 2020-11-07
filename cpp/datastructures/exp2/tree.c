#include <stdio.h>

int max(int a, int b)
{
  return a > b ? a : b;
}

int dfs(char *pre, char *in, int n)
{
  if (n == 0)
    return 0;

  int i;
  for (i = 0; i < n; i++)
  {
    if (in[i] == pre[0])
      break;
  }

  int left = dfs(pre + 1, in, i);
  int right = dfs(pre + i + 1, in + i + 1, n - i - 1);
  return max(left, right) + 1;
}

int main()
{
  int n;
  scanf("%d", &n);
  char pre[51], in[51];
  for (int i = 0; i <= n; i++)
  {
    scanf("%c", &pre[i]);
  }

  for (int i = 0; i <= n; i++)
  {
    scanf("%c", &in[i]);
  }

  printf("%d\n", dfs(pre + 1, in + 1, n));
  return 0;
}