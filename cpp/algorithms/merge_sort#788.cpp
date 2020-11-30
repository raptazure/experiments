#include<iostream>
using namespace std;
//给定范围100000可能爆int，用long long
typedef long long ll;

const int N = 1e6 + 10;
int n;
int q[N], tmp[N];
/*
1.左半边内部的逆序对数量：merge_sort(L, mid)
2.右半边内部的逆序对数量：merge_sort(mid + 1, R)
3.设右半边第一个数比左半边s1个数小，右半边第二个数比左边s2个数小...
  则跨边界的逆序对有s1+s2+...+sn个
4.回顾归并排序，用了两个指针移动，每次选择两个指针所指的较小的数填进辅助数组
  若左边指针指向i时右边指针指向j，且q[i]>q[j],则只有从i到左边尾部的数都大于q[j]
  sj就应为mid-i+1
*/
ll merge_sort(int l, int r)
{
    if(l >= r) return 0;
    int mid = l + r >> 1;
    ll res = merge_sort(l, mid) + merge_sort(mid + 1, r);
    //归并的过程
    int k = 0, i = l, j = mid + 1;
    while(i <= mid && j <= r)
        if(q[i] <= q[j]) tmp[k++] = q[i++];
        else
        {
            tmp[k++] = q[j++];
            res += mid - i + 1;
        }
        //扫尾
        while(i <= mid) tmp[k++] = q[i++];
        while(j <= r) tmp[k++] = q[j++];
        //物归原主
        for(int i = l, j = 0; i <= r; i++, j++) q[i] = tmp[j];
        return res;
}

int main()
{
    cin >> n;
    for (int i = 0; i < n; i++) cin >> q[i];
    cout << merge_sort(0, n - 1) << endl;
    return 0;
}