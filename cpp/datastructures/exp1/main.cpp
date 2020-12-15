#include <iostream>
#include <sstream>
#include <string>
#include <list>
using namespace std;

struct para
{
  list<string> content;

  void init()
  {
    content.clear();
    return;
  }

  void read()
  {
    string temp;
    getline(cin, temp);
    stringstream sin(temp);
    while (sin >> temp)
    {
      content.push_back(temp);
    }
  }

  void debug_print()
  {
    if (content.empty())
    {
      cout << "error" << endl;
    }
    while (!content.empty())
    {
      cout << content.front() << endl;
      content.pop_front();
    }
  }
};

int main()
{
  para solver;
  solver.init();
  solver.read();
  solver.debug_print();
  return 0;
}