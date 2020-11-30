#include <iostream>
#include <algorithm>
using namespace std;
const int N = 100010;
int n,q[N];

int main()
{
    scanf("%d",&n);
    for (int i = 0; i < n; i++) scanf("%d",&q[i]);
    sort(q, q+n);

    for (int i = 0; i < n; i++) printf("%d",q[i]);
    return 0;
}
