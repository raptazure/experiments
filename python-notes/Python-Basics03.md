---
title: Python Control Flow
categories: Python
---

# 控制语句

## 选择结构：

#### 单分支选择结构：

- if 条件表达式(逻辑，关系，算术)，注意缩进。

  ```python
  num = input("please input a number")
  if int(num)<10:
      print(num)
  ```
  
  <!-- more -->
  
- 条件表达式值为False的情况如下：False，0，0.0，None空值，空序列对象（空列表，空元组，空字典，空字符串），空range对象，空迭代对象,其他均为True。条件表达式中不能有赋值操作符‘=’(报语法错误)
  
  ```python
  if 3:
      print("ok")
  a = []
  if a:
      print("empty list,False")
  s = "False"
  if s:
      print("string,True")
  c = 9
  if 3<c<10:
      print("True")
  ‘’‘
  ok
  string,True
  True
  ’‘’
  ```
#### 多分支选择结构:

```python
#VS Code Python3.7.4
s = input("please input a number")
if int(s) < 10:
    print("s is smaller than 10")
else:
    print("s is bigger")
# 三元条件运算符
num = input("please input another number")
print(num if int(num) < 10 else "too big")

#多分支结构种分支有逻辑关系,不可随意颠倒顺序
score = int(input("please input your score "))
grade = ""
if score < 60:
    grade = 'E'
elif score < 80:
    grade = 'D'
elif score < 90:
    grade = 'B'
else:
    grade = 'A'
print("your score is {0} and your grade is {1}".format(score,grade))
# 多分支也可只用if,但推荐elif
# 选择结构嵌套注意缩进
score = int(input("please input your score "))
degree = 'ABCDE'
num = 0
if score > 100 or score < 0:
    print("please input a number between 0 and 100")
else:
    num = score//10
    if num < 5:
        num = 5
    print(degree[9-num])
```

## 循环结构：

```python
## while
num = 0
while num < 100:
    print(num,end = '\t')
    num += 1
## for  用于可迭代对象的遍历
for x in (20,30,40):
    print(x*3)
for x in "rapt":
    print(x)
d = {'name':'rapt','age':18}
for x in d:
    print(x)
for x in d.values():
    print(x)
for x in d.keys():
    print(x)
for x in d.items():
    print(x)
'''
name
age
rapt
18
name
age
('name', 'rapt')
('age', 18)
'''
sum_even = 0
sum_odd = 0
for num in range(101):
    if num%2==0:
        sum_even += num
    else:
        sum_odd += num
print("even sum = {0}, odd sum = {1}".format(sum_even,sum_odd))
## 嵌套循环
for x in range(5):
    for y in range(5):
        print(x,end = '\t')
    print()
'''
0	0	0	0	0	
1	1	1	1	1	
2	2	2	2	2	
3	3	3	3	3	
4	4	4	4	4	
'''
# 打印乘法表
for m in range(1,10):
    for n in range(1,m+1):
        print("{0} * {1} = {2}".format(m,n,(m*n)), end = '\t')
    print()
# 与字典配合
r1 = dict(name='rapt1',age=18)
r2 = dict(name='rapt2',age=19)
r3 = dict(name='rapt3',age=20)
table = [r1,r2,r3]
for x in table:
    if x.get('age') >= 19:
        print(x)
# break结束整个循环 & continue结束本次循环进入下一次
empNum = 0
salarySum = 0
salary = []
while True:
    s = input("Please enter the salary:")
    if s.upper() == 'Q':
        print("quit")
        break
    if float(s) < 0:
        continue
    empNum += 1
    salary.append(float(s))
    salarySum += float(s)
print("the number of employees:{0}".format(empNum))
print("salary:",salary)
print(f"average salary:{salarySum/empNum}")
# while，for循环可以附带一个else语句，如果for，while没有被break结束，则会执行else子句
empNum = 0
salarySum = 0
salary = []
for i in range(4):
    s = input("Please enter the salary:")
    if s.upper() == 'Q':
        print("quit")
        break
    if float(s) < 0:
        continue
    empNum += 1
    salary.append(float(s))
    salarySum += float(s)
else:
    print("you have entered salary of all the employees")
print("the number of employees:{0}".format(empNum))
print("salary:",salary)
print(f"average salary:{salarySum/empNum}")
```

- 循环代码优化：

  - 尽量减少循环内部不必要的计算
- 嵌套循环中尽量减少内层循环计算，尽可能外提
  - 局部变量查询较快，尽量使用局部变量
- 连续多个字符串，使用`join()`不用+
  - 列表进行元素插入和删除，尽量在列表尾部操作

 ```python
import  time
start = time.time()
for i in range(1000):
    result = []
    for m in range(10000):
        result.append(i*1000+m*100)
end = time.time()
print(f'time cost:{end - start}')

start2 = time.time()
for i in range(1000):
    result = []
    c = i*1000 
    for m in range(10000):
        result.append(c+m*100)
end2 = time.time()
print(f"time cost:{end2 - start2}")
# time cost:1.810370922088623
# time cost:1.4288480281829834
 ```

- `zip()`进行并行迭代：

  ```python
  names = ['rapt','azure','linux']
  ages = [18,19,50]
  jobs = ['stu','pro','opt']
  
  for name,age,job in zip(names,ages,jobs):
      print("{0}--{1}--{2}".format(name,age,job))
  # 相同功能也可通过以下代码实现
  for i in range(3):
      print("{0}--{1}--{2}".format(names[i],ages[i],jobs[i]))
  
  '''
  rapt--18--stu
  azure--19--pro
  linux--50--opt
  '''
  ```
  
- 推导式创建序列：推导式是从一个或多个迭代器快速创建序列的一种方法，它可以将循环和条件判断结合，从而简化代码。

  ```python
  # 列表推导式
  # [表达式 for item in 可迭代对象 if 条件判断]
  >>> [x for x in range(1,5)]
  [1, 2, 3, 4]
  >>> [x * 2 for x in range(1, 20) if x % 5 == 0]
  [10, 20, 30]
  >>> [a for a in 'abcdef']
  ['a', 'b', 'c', 'd', 'e', 'f']
  cells = [(row, col) for row in range(1, 10) for col in range(1, 10)]
  for cell in cells:
      print(cell)
  # (1,1) to (9,9)
  
  # 字典推导式 
  # {key_exp:value_exp for 表达式 in 可迭代对象}
  my_text = 'i love programming, i love computers'
  char_count = {c:my_text.count(c) for c in my_text}
  print(char_count)
  # {'i': 3, ' ': 5, 'l': 2, 'o': 4, 'v': 2, 'e': 3, 'p': 2, 'r': 3, 'g': 2, 'a': 1, 'm': 3, 'n': 1, ',': 1, 'c': 1, 'u': 1, 't': 1, 's': 1}
  
  #集合推导式 -> 没有key的字典
  #{表达式 for item in 可迭代对象 if 条件判断}
  >>> {x for x in range(1,100) if x%9 == 0}
  {99, 36, 72, 9, 45, 81, 18, 54, 90, 27, 63}
  
  #生成器推导式：生成元组
  >>> (x for x in range(1,100) if x%9 == 0)
  <generator object <genexpr> at 0x7fddcf259e50>
  #提示“一个生成器对象，可见元组没有推导式
  gnt = (x for x in range(1,100) if x % 9 == 0)
  print(tuple(gnt))
  #(9, 18, 27, 36, 45, 54, 63, 72, 81, 90, 99)
  #生成器是可迭代对象
  >>> gnt = (x for x in range(1,100) if x%9 == 0)
  >>> for x in gnt:
  	print(x, end = ' ' )
  
  ```
## 小练习：多彩同心圆

```python
import turtle

my_colors = ("red", "green", "blue", "yellow")
t = turtle.Pen()
t.width(5)
t.speed(0)
for i in range(30):
    t.penup()
    t.goto(0,- i * 10)
    t.pendown()
    t.color(my_colors[i % len(my_colors)])
    t.circle(15 + i * 10)
turtle.done()
```



​			





  

  

