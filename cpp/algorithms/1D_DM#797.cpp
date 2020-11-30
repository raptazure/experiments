//差分法->前缀和的逆运算：已知数组{an}，构造{bn}使得ai=b1+b2+...+bn
//bn是an的差分，an是bn的前缀和。差分可以用O(1)的时间给原数组中某一段的元素[l,r]都加上一个固定值c
//只需要b[l]+c,b[r+1]-c
//假设初始时a[i] = 0
#include <iostream>

using namespace std;

const int N = 100010;
int n, m;
int a[N], b[N];

void insert(int l, int r, int c)
{
    b[l] += c;
    b[r + 1] -= c;
}
int main()
{
    scanf("%d%d",&n ,&m);
    for (int i = 1; i <= n; i++) scanf("%d",&a[i]);
    for (int i = 1; i <= n ; i++) insert(i, i, a[i]);
    while(m--)
    {
        int l, r, c;
        scanf("%d%d%d",&l, &r, &c);
        insert(l, r, c);
    }

    for (int i = 1; i <= n; i++) b[i] += b[i - 1];
    for (int i = 1; i <= n; i++) printf("%d ",b[i]);
    return 0;
}