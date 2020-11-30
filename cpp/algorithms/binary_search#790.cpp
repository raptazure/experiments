#include<iostream>
using namespace std;
int main()
{
    double n;
    int flag = 0;
    cin >> n;
    if(n <= 0) 
    {
        flag = 1;
        n = -n;
    }
    double l = 0, r = n;
    for (int i = 0; i < 100; ++i)
    {
        double mid = (r + l) / 2;
        if(mid * mid * mid >= n)  r = mid;
        else l = mid;
    }
    if(flag) printf("-");
    printf("%.6lf",l);
    return 0;
}