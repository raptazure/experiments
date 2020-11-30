#include<iostream>
using namespace std;

const int N = 1e6 + 10;

int n;
int q[N];
void quick_sort(int q[],int l ,int r)
{
    if(l >= r)return;//判边界，区间没有数-有序
    int x = q[l], i = l - 1, j = r + 1;//两个指针向两边扩一格
    while(i < j)
    {
        do i++; while (q[i] < x);
        do j--; while (q[j] > x);
        if (i < j) swap(q[i], q[j]);//两个指针没有相遇-交换
    }
    quick_sort(q, l, j);
    quick_sort(q, j+1, r);
}
int main()
{
    scanf("%d",&n);
    for (int i = 0; i < n; i++) scanf("%d",&q[i]);
    quick_sort(q,0,n-1);
    for (int i = 0; i < n; i++) printf("%d ",q[i]);
    return 0;

}
/*
AcWing 785
5
3 1 2 4 5
1 2 3 4 5
*/