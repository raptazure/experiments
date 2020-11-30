#include<iostream>
using namespace std;
const int N = 1e6 + 10;
int q[N], n;

void selection_sort(int q[], int n)
{
    for (int i = 0; i < n; ++i)
    {
        int minKey = q[i];
        for (int j = i; j < n; j++)
            if(minKey > q[j])
                swap(q[j], minKey);
        q[i] = minKey;
    }
}
int main()
{
    scanf("%d",&n);
    for (int i = 0; i < n; ++i) scanf("%d",&q[i]);
    selection_sort(q, n);
    for (int i = 0; i < n; ++i) printf("%d ",q[i]);
    return 0;
}