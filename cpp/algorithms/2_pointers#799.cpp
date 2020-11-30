/*
//朴素做法：O(n^2)
for(int i = 0; i < n; i++)
    for(int j = 0; j <= i; j++)
        if(check(j, i)
        {
            res = max(res, i - j + 1);
        }
发现一个指针向后移动时，另一个指针肯定也是后移(具有单调性)
右边的指针为i，左边为j，j往左最远走到有重复元素时停下，当i向右走时，j不可能往左走，所以单调
//双指针算法：O(n)
for(int i = 0, j = 0; i < n; ++i)
{
    while(j <= i && check(j, i)) j++;
    res = max(res, i - j + 1);
}
s[N] 在[j, i]区间，i右移时s[a[i]]++, j右移时s[a[j]]--动态统计区间有几个数
*/
#include <iostream>
using namespace std;
const int N = 100010;
int a[N], s[N], n;
int main()
{
    cin >> n;
    for(int i = 0; i < n; i++) cin >> a[i];
    int res = 0;
    for(int i = 0, j = 0; i < n; i++)
    {
        s[a[i]]++;
        while(j <= i && s[a[i]] > 1)
        {
            s[a[j]]--;
            j++;
        }
        res = max(res, i - j + 1);
    }
    cout << res << endl;
    return 0;
}
/*重理思路：本题的核心思想是双指针算法，i指针是快指针，j是慢指针，每当i指针i指针右移一位的时候，
  其对应的元素的个数就需要++，可以看出此处需要一个计数的数组或者unordered_map<int, int>，
  如果是字符串或者字符等元素的话，建议用后者。
  接下来介绍check函数，check函数的实现功能就是，当s[a[i]]>1的话，我们就持续j++，
  并且将s[a[j]]--，这一步绝对不能省。
  第三步就是最后的操作了res = max(res, i - j + 1);
 */

//先写一个暴力n2的做法，观察i和j是否有单调关系，如果有就利用这种关系将整个枚举状态数量从n2优化到n

