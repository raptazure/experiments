/*    双指针算法：
比如归并排序两个指针指向两个序列，又如快速排序中两个指针指向一个序列，双指针维护一段区间
for(int i = 0; i < n; ++i)
{
    while(j < i && check(i, j)) j++: 
    //每道题目的具体逻辑      
}
 核心思想：双指针可将两层嵌套循环O(n^2)的朴素算法优化为O(n)
 for(int i=0; i<n; i++)
    for(int j=0; j< n; j++)
*/
//输入一个字符串(单词以一个空格隔开)，输出其中的每个单词，每个单词占一行
#include <iostream>
#include <cstring>
using namespace std;
int main()
{
    char str[1000];
    fgets(str, sizeof(str), stdin);
    int n = strlen(str);
    for (int i = 0; i < n; i++)
    {
        int j = i;
        while(j < n && str[j] != ' ') j++;
        //这道题的具体逻辑
        for(int k = i; k < j; k++) cout << str[k];
        cout << endl;
        i = j; //跳过这个单词区间
    }
    return 0;
}