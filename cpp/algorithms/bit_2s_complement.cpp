/*原码 -> 补码
  补码为何为 ~x + 1？ (-x) + x = 0 -> -x = 0 - x
  32个0 - x -> 借位100...0(32个0) - x = ～x + 1
*/
#include<iostream>
using namespace std;
int lowbit(int x)
{
    return x & -x;
}
int main()
{
    int n = 10;
    unsigned int x = -n;

    for(int i = 31; i >= 0; i--) 
        cout << (x >> i & 1);
    cout << endl;
    return 0;
}