/*前缀和数组中的第i个元素Si=a1+a2+...+ai
前缀和一般从S1开始，把边界S0定义为 0
1.如何求Si for (i = 1; i <= n; i++)   S[i] = S[i-1] + ai;  S0 = 0; 
2.作用：快速求出数组中一段数的和 如[l, r] -> Sr - S(l-1) 
*/
#include <iostream>
using namespace std;

const int N = 1e6 + 10;

int n, m;
int a[N], s[N];
int main()
{
    scanf("%d%d",&n ,&m);
    for (int i = 1; i <= n; ++i) scanf("%d",&a[i]);
    for (int i = 1; i <= n; ++i) s[i] = s[i - 1] + a[i]; //前缀和的初始化
    while(m--)
    {
        int l, r;
        scanf("%d%d", &l, &r);
        printf("%d\n",s[r] - s[l - 1]); //区间和计算
    }  
    return 0;
}
