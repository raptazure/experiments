#include<iostream>
using namespace std;
const int N = 100010;
int n, m, x, ansi, ansj;
int a[N], b[N];
int main()
{
    int i, j;
    cin >> n >> m >> x;
    for(int i = 0; i < n; ++i) cin >> a[i];
    for(int i = 0; i < m; ++i) cin >> b[i];
    for(i = 0, j = m - 1; i < n && j >= 0;)
    {
        if(a[i] + b[j] != x){
            if(a[i] + b[j] > x) j--;
            else i++;
        }else break;
    }
    cout << i << " " << j << endl;
    return 0;
}

