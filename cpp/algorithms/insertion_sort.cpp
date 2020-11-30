#include<iostream>
using namespace std;

const int N = 1e6 + 10;
int q[N], n;

void insertion_sort(int q[], int n)
{
    int j;
    for (int i = 1; i < n; ++i)
    {
        j = i; 
        while (j > 0 && q[j - 1] > q[j])
        {
            swap(q[j], q[j - 1]);
            j--;
        }
    }
}
int main()
{
    scanf("%d",&n);
    for (int i = 0; i < n; ++i) scanf("%d",&q[i]);
    insertion_sort(q, n);
    for (int i = 0; i < n; ++i) printf("%d ",q[i]);
    return 0;
}