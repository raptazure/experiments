//高精度加法，Java和Python不需要考虑
#include<iostream>
#include<vector>
//vector自带size函数，不需要开额外变量来存数组长度了
using namespace std;
//加引用避免再拷贝整个数组
vector<int> add(vector<int> &A, vector<int> &B) 
{
    //定义储存结果的vector以及进位t，第0位没有进位->初始化为0
    vector<int> C;
    int t = 0;
    //从个位开始遍历直至遍历完A和B的所有位
    for (int i = 0; i < A.size() || i < B.size(); i++)
    {
        //每一次用t表示Ai，Bi与上一个数的进位  三个数的和
        if(i < A.size()) t += A[i];
        if(i < B.size()) t += B[i];
        //当前这一位输出t除以10的余数
        C.push_back(t % 10);
        //t是否进位
        t /= 10;
    }
    //如果最高位有进位则补1
    if(t) C.push_back(1);
    return C;
}
int main()
{
    //使用字符串读入
    string a, b;
    vector<int> A, B;
    cin >> a >> b; //a = "123456"
    //使用vector逆序读入,变成整数需要减去偏移量0
    for (int i = a.size() - 1; i >= 0; i--) A.push_back(a[i] - '0');//A = [6,5,4,3,2,1]
    for (int i = b.size() - 1; i >= 0; i--) B.push_back(b[i] - '0');
    //自动判断类型，相当于vector<int>
    auto C = add(A, B);
    //倒序输出
    for (int i = C.size() - 1; i >= 0; i--) printf("%d", C[i]);
    return 0;
}

//个位放在0的位置：出现进位的情况时需要在高位补上1,这样在数组后端补数比较容易
