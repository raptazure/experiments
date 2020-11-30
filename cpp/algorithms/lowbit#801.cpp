/*
lowbit(x):返回x的最后一位1的位置
x = 1010; lowbit(x) = 1; x & (-x)
原理：-x = ~x + 1 -> x & (~x + 1) 举例理解
应用：统计x里1的个数 -> 每次把x的最后一个1去掉
*/
#include <iostream>
using namespace std;
int lowbit(int x)
{
    return x & -x;
}
int main()
{
    int n;
    cin >> n;
    while(n --)
    {
        int x;
        cin >> x;

        int res = 0;
        //每次减去x的最后一位1
        while(x) x -= lowbit(x), res++;
        cout << res << " ";
    }
    return 0;
}