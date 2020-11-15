#include <stdio.h>
#include <stdlib.h>

typedef struct Node
{
  int coef; // coefficient
  int exp;  // exponent
  struct Node *next;
} NODE, *PNODE;

int main()
{
  int coef, exp;
  PNODE pHead, pTail, pTemp;

  pHead = (PNODE)malloc(sizeof(NODE));
  pTail = pHead;

  // EOF - Ctrl + D in macOS
  while (scanf("%d%d", &coef, &exp) != EOF)
  {
    pTemp = (PNODE)malloc(sizeof(NODE));
    pTemp->coef = coef;
    pTemp->exp = exp;
    pTail->next = pTemp;
    pTail = pTemp;
  }

  pTail->next = NULL;
  pTemp = pHead->next;

  if (pTemp->exp == 0)
  {
    printf("0 0");
  }
  else
  {
    while (pTemp)
    {
      if (pTemp->exp > 0)
      {
        if (pTemp != pHead->next)
          putchar(' ');
        printf("%d %d", pTemp->coef * pTemp->exp, pTemp->exp - 1);

        pTemp = pTemp->next;
      }
      else
        break;
    }
  }

  return 0;
}