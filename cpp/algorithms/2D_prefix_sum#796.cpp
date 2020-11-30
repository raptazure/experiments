//求子矩阵的和：容斥原理
#include <iostream>
using namespace std;
const int N = 1010;
int n, m, q;
int a[N][N], s[N][N];
int main()
{ 
    scanf("%d%d%d", &n, &m, &q);
    for (int i = 1; i <= n; i++)
        for (int j = 1; j <= m; j++)
            scanf("%d",&a[i][j]);
    
    for (int i = 1; i <= n; i++)
        for (int j = 1; j <= m; j++)
            s[i][j] = s[i - 1][j] + s[i][j - 1] - s[i - 1][j - 1] + a[i][j];//求前缀和
    //q次询问
    while (q--)
    {
        int x1, y1, x2, y2;
        scanf("%d%d%d%d", &x1, &y1, &x2, &y2);
        printf("%d\n", s[x2][y2] - s[x1 -1][y2] - s[x2][y1 -1] + s[x1 - 1][y1 - 1]);//计算子矩阵和
    }
    return 0;
}