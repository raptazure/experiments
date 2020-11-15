#include <iostream>
#include <set>
using namespace std;
int N, M;
set<string> File[110];

void extractword(string word, set<string> &File)
{
  for (int i = 0; i < word.length(); i++)
  {
    if (isalpha(word[i]))
      word[i] = tolower(word[i]);
  }
  for (int i = 0; i < word.length(); i++)
  {
    int len = 0, ptri = i;
    if (isalpha(word[i]))
    {
      while (isalpha(word[i]))
      {
        if (len >= 10)
        {
          word[i] = ' ';
        }
        else
          len++;
        i++;
      }
      if (len >= 3 && len <= 10)
      {
        File.insert(word.substr(ptri, len));
      }
    }
  }
}

int main()
{
  cin >> N;
  for (int i = 1; i <= N; i++)
  {
    string word;
    while ((getline(cin, word) && word != "#"))
    {
      extractword(word, File[i]);
    }
  }
  cin >> M;
  for (int i = 0; i < M; i++)
  {
    int f1, f2;
    int same = 0, n1, n2;
    scanf("%d%d", &f1, &f2);
    n1 = File[f1].size();
    n2 = File[f2].size();
    for (set<string>::iterator it = File[f1].begin(); it != File[f1].end(); it++)
    {
      if (File[f2].find(*it) != File[f2].end())
      {
        same++;
      }
    }
    double x = same, y = n1 + n2 - same;
    printf("%.1lf%%\n", x / y * 100);
  }
  return 0;
}