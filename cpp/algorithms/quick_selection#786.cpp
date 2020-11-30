//AcWing 786  quick_selection
#include<iostream>
using namespace std;
int n, k;
const int N = 1e6+10;
int q[N];
int quick_sort(int l, int r, int k)
{
    if (l >= r) return q[l];
    int x = q[l], i = l - 1, j = r + 1;
    while (i < j)
    {
        while(q[++i] < x);
        while(q[--j] > x);
        if (i < j) swap(q[i],q[j]);
    }
    int sl = j - l + 1;
    if(k <= sl) return quick_sort(l, j, k);
    return quick_sort(j + 1, r, k - sl);
}
int main()
{
    cin >> n >> k;
    for (int i = 0; i < n;i++)
        scanf("%d",&q[i]);
    quick_sort(0, n - 1, k);
    printf("%d",q[k - 1]);
    return 0;
}
