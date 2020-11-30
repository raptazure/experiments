#include<iostream>
using namespace std;
const int N = 1e6 + 10;
int q[N], n;

void bubble_sort(int q[], int n)
{
    for (int i = 0; i < n - 1; ++i)
    {
        for (int j = 0; j < n - i -1; j++)
        {
            if(q[j] > q[j + 1]) swap(q[j], q[j + 1]);
        }
    }
}
int main()
{
    scanf("%d",&n);
    for (int i = 0; i < n; ++i) scanf("%d",&q[i]);
    bubble_sort(q, n); 
    for (int i = 0; i < n; ++i) printf("%d ",q[i]);
    return 0;
}