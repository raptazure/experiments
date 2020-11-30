//浮点数二分没有边界问题，相对容易解决
#include<iostream>
using namespace std;
int main()
{
    double x;
    cin >> x;
    
    double l = 0, r = x;
    /* 不用精度判断而是直接迭代100次
    for (int i = 0; i < 100; i++)
    {
        double mid = (r + l) / 2;
        if(mid * mid >= x) r = mid;
        else l = mid;
    }
    */
    while (r - l > 1e-8)
    {
        double mid = (r + l) / 2;
        if(mid * mid >= x) r = mid;
        else l = mid;
    }
    printf("%lf\n",l);
    return 0;
}