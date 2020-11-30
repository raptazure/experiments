/*
n的二进制表示中第k位是几
n = 15 = 1111, 把个位记为k = 0
1.先把第k位数字移到最后一位 n >> k
2.看个位是几 x & 1
即 n >> k & 1
*/
#include <iostream>
using namespace std;
int main()
{
    int n = 10;
    for(int k = 3; k >= 0; k--) cout << (n >> k & 1);
    return 0;
}