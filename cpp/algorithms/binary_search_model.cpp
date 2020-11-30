//mid属于左边：区间[1, r]被划分为[1, mid]和[mid+1, r]时使用
int bsearch_1(int l, int r)
{
    while (l < r)
    {
        int mid = (l + r) / 2;
        if(check(mid)) r = mid; 
        //check()判断mid是否满足性质
        else l = mid + 1;
    }
    return 1;
}
//mid属于右边：区间[1, r]被划分为[1, mid - 1]和[mid, r]时使用
int bsearch_2(int l, int r)
{
    while (l < r)
    {
        int mid = (l + r + 1) / 2;
        if(check(mid)) l = mid;
        else r = mid - 1;
    }
    return 1;
}