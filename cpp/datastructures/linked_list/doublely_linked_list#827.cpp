#include <iostream>
using namespace std;

const int N = 100010;

int m;
int e[N], l[N], r[N], idx;

void init()
{
    // 0表示左端点，1表示右端点
    r[0] = 1, l[1] = 0;
    idx = 2;
}

// 在第k个点右边插入x
// add(l[k], x)可以在k的左边插入x
void add(int x, int k)
{
    e[idx] = x;
    r[idx] = r[k];
    l[idx] = k;
    l[r[k]] = idx;
    r[k] = idx;
}

// 删除第k个点
void remove(int k)
{
    r[l[k]] = r[k];
    l[r[k]] = l[k];
}