#include <iostream>
#include <algorithm>
#include <cstring>
using namespace std;

struct Student
{
  int id;
  char name[9];
  int grade;
};

Student stu[110];

bool byId(Student a, Student b)
{
  return a.id < b.id;
}
bool byName(Student a, Student b)
{
  if (strcmp(a.name, b.name) == 0)
    return a.id < b.id;
  return strcmp(a.name, b.name) < 0;
}
bool byGrade(Student a, Student b)
{
  if (a.grade == b.grade)
    return a.id < b.id;
  return a.grade < b.grade;
}

int main()
{
  int N, C;
  cin >> N >> C;
  for (int i = 0; i < N; i++)
  {
    cin >> stu[i].id >> stu[i].name >> stu[i].grade;
  }

  switch (C)
  {
  case 1:
    sort(stu, stu + N, byId);
    break;
  case 2:
    sort(stu, stu + N, byName);
    break;
  case 3:
    sort(stu, stu + N, byGrade);
  default:
    break;
  }

  cout << ">>= Sorted:\n";

  for (int i = 0; i < N; ++i)
    printf("%.6d %s %d\n", stu[i].id, stu[i].name, stu[i].grade);
  return 0;
}
