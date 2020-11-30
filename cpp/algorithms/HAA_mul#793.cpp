#include<iostream>
#include<vector>
using namespace std;
//C = A * b
vector<int> mul(vector<int> &A, int b)
{
    vector<int> C;
    //t进位
    int t = 0;
    //i未循环完||进位没处理完(t!=0)时一直循环
    for (int i = 0; i < A.size() || t; i++)
    {
        if(i < A.size()) t += A[i] * b;
        //输出当位
        C.push_back(t % 10);
        //整除10得到进位
        t /= 10;
    }
    return C;
}
int main()
{
    string a;
    int b;
    cin >> a >> b;
    vector<int> A;
    
    for (int i = a.size() - 1; i >= 0; --i) A.push_back(a[i] - '0');
    auto C = mul(A, b);
    
    for (int i = C.size() - 1; i >= 0; --i) printf("%d",C[i]);
    return 0;
}