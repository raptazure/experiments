#include<iostream>
#include<vector>

using namespace std;
//判断是否有 A >= B
bool cmp(vector<int> &A, vector<int> &B)
{
    //先判断位数
    if (A.size() != B.size()) return A.size() > B.size();
    //位数相同从高位开始比
    for (int i = A.size() - 1; i >= 0; i--)
        if(A[i] != B[i])
            return A[i] > B[i];
        //所有位都相等返回true
    return true;
}
//C = A - B
vector<int> sub(vector<int> &A, vector<int> &B)
{
    vector<int> C;
    for (int i = 0, t = 0; i < A.size(); i++)
    {
        //当前这一位的值
        t = A[i] - t;
        if (i < B.size()) t -= B[i];
        //输出该位  t>=0 -> t t<0 -> t+10    
        C.push_back((t + 10) % 10);
        //t<0借位
        if (t < 0) t = 1;
        else t = 0;
    }
    //去掉前导0,防止出现003的情况。如果C只有一位，则不能把前面的0去掉
    while (C.size() > 1 && C.back() == 0) C.pop_back();
    return C;
}

int main()
{
    string a, b;
    vector<int> A, B;
    cin >> a >> b;
    for (int i = a.size() - 1; i >= 0; i--) A.push_back(a[i] - '0');
    for (int i = b.size() - 1; i >= 0; i--) B.push_back(b[i] - '0');
    if (cmp(A, B))
    {
        auto C = sub(A, B);
        for (int i = C.size() - 1; i >= 0; i--) printf("%d",C[i]);
    }
    else 
    {
        auto C = sub(B, A);
        printf("-");
        for (int i = C.size() - 1; i >= 0; i--) printf("%d",C[i]);
    }
   return 0;
}