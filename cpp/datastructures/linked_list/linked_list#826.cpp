#include <iostream>
using namespace std;

const int N = 100010;

int head, e[N], ne[N], idx;

void init()
{
    head = -1;
    idx = 0;
}

// 将x插到头节点
void add_to_head(int x)
{
    e[idx] = x, ne[idx] = head, head = idx++;
}

// 将x插入到下标为k的点后
void add(int k, int x)
{
    e[idx] = x, ne[idx] = ne[k], ne[k] = idx++;
}

// 将下标为k的点后面的点删掉
void remove(int k)
{
    ne[k] = ne[ne[k]];
}

// 观察idx得第k个插入的数： 下标为k-1的节点
// 0号点是第一个插入的点
int main()
{
    int m;
    cin >> m;
    init();
    while(m--)
    {
        int k, x;
        char op;
        cin >> op;
        if(op == 'H')
        {
            cin >> x;
            add_to_head(x);
        }
        else if(op == 'D')
        {
            cin >> k;
            // k=0时删除头节点
            if(!k) head = ne[head];
            remove(k - 1);
        }
        else
        {
            cin >> k >> x;
            add(k - 1, x);
        }
    }
    for(int i = head; i != -1; i = ne[i])
        cout << e[i] << ' ';
    cout << endl;
    return 0;
}