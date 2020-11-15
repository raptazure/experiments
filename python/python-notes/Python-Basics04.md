---
title: Python Function
categories: Python
---
# 函数
## 基本概念：

- 函数代表一个任务或者一个功能，函数是代码复用的通用机制。

- 函数分类：内置函数，标准库函数，第三方库函数，用户自定义函数。

  <!--more-->

## 函数定义和调用：

- Python执行def会创建一个新的函数对象(对象在堆里)，并绑定到函数名变量(在栈里)上 -> 把对象地址给变量，可通过函数名变量找到对象。

- 圆括号内是形式参数表，有多个参数时应用`，`隔开。形参不需要声明变量类型，也不需要指定函数返回值类型，实参列表要与形参对应。

- return 返回值，不含return则返回None，要返回多个值可用序列存储。

- 调用函数前必须先定义函数(用def创建函数对象)：

  - 内置函数对象会自动创建
  - 标准库和第三方库函数，通过import导入模块时，会执行模块中的def语句。5

  ```python
  def my_func():
      print("$" * 8)
  for i in range(5):
      my_func()
  def printMax(a,b):
      '''compare two numbers and give the bigger one'''
      if a > b:
          print(a,"is bigger")
      else:
          print(b,"is bigger")
      
  printMax(10,20)
  printMax(2,34)
  # 打印文档字符串
  help(printMax)
  
  def test(x,y,z):
      return (x* 10, y * 10, z * 10)
  
  print(test(19, 29, 39))
  # (190, 290, 390)
  ```

```python
def test():
    print("object")
test()
c = test
c()
print(id(test))
print(id(c))
print(type(c))
'''
object
object
139964785728976
139964785728976
<class 'function'>
'''
```

- 全局变量：在函数和类定义之外声明的变量，作用域是定义的模块，一般做常量使用，函数内要改变全局变量的值需用global声明。

  局部变量：在函数体中声明的变量(包含形参)，局部变量存在栈帧里(Stack Frame)，局部变量的引用快于全局变量。若函数内局部变量与全局变量同名，则在函数内屏蔽全局变量。

  使用`print(locals())` `print(globals())`打印所有局部变量和全局变量

  ```python
  # 测试局部变量与全局变量效率
  import math
  import time
  def test():
      start = time.time()
      for i in range(100001):
          math.sqrt(30)
      end = time.time()
      print("time cost: {}".format(end - start))
  def test2():
      b = math.sqrt
      start = time.time()
      for i in range(100001):
          b(30)
      end = time.time()
      print("time cost: {}".format(end - start))
  test()
  test2()
  # time cost: 0.010730981826782227
  # time cost: 0.005475521087646484
  ```

- 参数传递：本质是从实参到形参的赋值操作，Python中一切皆对象，所有赋值操作都是引用的赋值，所以python中的参数传递是“引用传递”而非“值传递”。具体操作分为：

  - 对可变对象(字典，列表，集合，自定义的对象等)进行“写操作”，直接作用于原对象本身。传递参数是可变对象时，实际传递的是对象的引用，在函数中不创建新的对象拷贝，而是可以直接修改所传递的对象。

    ```python
    b = [10, 20]
    def f2(m):
        print("m",id(m))
        m.append(30)
    f2(b)
    print("b",id(b))
    print(b)
    '''
    m 140456535695920
    b 140456535695920
    [10, 20, 30]
    '''
    ```

  - 对不可变对象(数字，字符串，元组，布尔值，function等)进行“写操作”，会产生一个新的“对象空间”，并用新的值填充这块空间(起到其他语言的“值传递”效果但不是“值传递”)。传递对象是不可变对象时，实际传递的也是对象的引用，在“赋值”操作时，由于不可变对象无法修改，系统会创建一个新的对象拷贝，此时用的是浅拷贝。

    ```python
    a = 10000
    def f(n):
        print("n",id(n))
        n = n + 200
        print("n",id(n))
        print(n)
    f(a)
    print("a",a,id(a))
    '''
    n 140526911552880
    n 140526912199440
    10200
    a 10000 140526911552880
    '''
    #n和a一开始是同一个对象，n值改变->新的对象
    a = 10
    print("a:",id(a))
    def test(m):
        print("m:",id(m))
        m = 20
        print("m:",id(m))
    test(a)
    '''
    a: 139863371449600
    m: 139863371449600
    m: 139863371449920
    '''
    # 浅拷贝
    import copy
    a = (10, 20, [5, 6])
    print("a:",id(a))
    def test(m):
        print("m:",id(m))
        m[2][0] = 233
        print(m)
        print("m:",id(m))
    test(a)
    print(a)
    '''
    a: 140400765861072
    m: 140400765861072
    (10, 20, [233, 6])
    m: 140400765861072
    (10, 20, [233, 6])
    '''
    #传递不可变对象时，若不可变对象里包含的子对象是可变的，在方法内修改这个可变对象时，源对象也会发生变化
    ```
  
- 浅拷贝和深拷贝：内置函数`copy`  `deepcopy`

  - 浅拷贝：不拷贝子对象内容，只是拷贝子对象的引用。

  - 深拷贝：将子对象的内存全部拷贝一份，对子对象的修改不会影响源对象。

    ```python
    import copy
    #浅拷贝
    a = [10, 20, [5, 6]]
    b = copy.copy(a)
    print("a",a)
    print("b",b)
    b.append(30)
    b[2].append(7)
    print("copy...")
    print("a",a)
    print("b",b)
    '''
    a [10, 20, [5, 6]]
    b [10, 20, [5, 6]]
    copy...
    a [10, 20, [5, 6, 7]]
    b [10, 20, [5, 6, 7], 30]
    '''
    a = [10, 20, [5, 6]]
    b = copy.deepcopy(a)
    print("a",a)
    print("b",b)
    b.append(30)
    b[2].append(7)
    print("deepcopy...")
    print("a",a)
    print("b",b)
    '''
    a [10, 20, [5, 6]]
    b [10, 20, [5, 6]]
    deepcopy...
    a [10, 20, [5, 6]]
    b [10, 20, [5, 6, 7], 30]
    '''
    ```
  
- 参数的几种类型：
  
  - 位置参数：函数调用时实参按默认位置顺序传递，需要个数和形参匹配。按位置传递的参数是位置参数。
  
  - 默认值参数：为某些参数设定默认值，这样这些参数在传递时就是可选的。默认值参数放在位置参数后面。
  
    ```python
    def f(a, b, c = 10):
        print(a, b, c)
    f(1, 2)
    ```
  
  - 命名参数：按照形参的名称传递参数，也叫关键字函数。
  
    ```python
    def f(a, b, c):
        print(a, b, c)
    f(c = 10, a = 20, b = 30)
    ```
    
  - 可变参数：数量可变。`*param`将多个参数收集到一个元组对象中。`**param`将多个参数收集到一个字典对象中。
  
    ```python
    def func(a, *b, **c):
        print(a, b, c)
    func(8, 9, 233, 1, name = 'rapt', age = 18)
    '''
    8 (9, 233, 1) {'name': 'rapt', 'age': 18}
    '''
    ```
  
  - 强制命名参数：在带*的可变参数后增加新的参数，必须在调用时“强制命名参数”。否则前面的可变参数会持续收集使得后面的无法进行赋值。

## lambda表达式与匿名函数：

- lambda表达式是一种简单的，在同一行中定义函数的方法。可用来声明匿名函数，lambda函数实际生成一个函数对象。运算结果是函数返回值

  `lambda arg1, arg2, arg3 … : <exp>`

  ```python
  f = lambda a, b, c : a + b + c
  print(f)
  print(f(2, 3, 4))
  g = [lambda a : a * 2, lambda b : b * 3, lambda c : c * 4]
  print(g[0](6), g[1](7), g[2](10))
  def test(a, b, c, d):
      return a + b + c + d
  h = [test, test] #函数也是对象
  print(h[0](2, 3, 4, 5))
  '''
  <function <lambda> at 0x7fbf0647b050>
  9
  12 21 40
  14
  '''
  ```

## `eval()` 函数：

- 功能：将字符串str当成有效的表达式来求值并返回计算结果。

- 语法：`eval([__source], [__globals], [__locals]) -> value`

- 参数：source - 一个Python表达式或函数compile()返回的代码对象

  ​			globals - 可选，必须是dictionary

  ​			locals - 可选，任意映射对象。

  ```python
  s = "print('rapt')"
  eval(s)
  a = 10
  b = 20
  c = eval("a + b")
  print(c)
  dict = dict(a = 100, b = 200)
  d = eval("a + b", dict)
  print(d)
  '''
  rapt
  30
  300
  '''
  ```
## 递归函数：

- 在函数内部直接或间接地调用自己的函数。类似于数学归纳法。每个递归函数需要包含终止条件和递归步骤两个部分。递归函数会创建大量函数对象，消耗较多内存、空间和运算能力，不建议在处理大量数据时使用。

  ```python
  def test(n):
      print("Testing...",n)
      if n == 0:
          print("over")
      else:
          test(n - 1)
      print("Testing****",n)
  test(3)
  '''
  Testing... 3
  Testing... 2
  Testing... 1
  Testing... 0
  over
  Testing**** 0
  Testing**** 1
  Testing**** 2
  Testing**** 3
  '''
  # 后进先出
  ```
  
- 递归计算阶乘：

  ```python
  def fact(n):
      if n == 1:
          return 1
      else:
          return  n * fact(n - 1)
  print("the result is",fact(5))
  ```
## 嵌套函数：

- 在函数内部定义的函数:

  ```python
  def outer():
      print("outer running")
      def inner():
          print("inner running")
      inner()
  outer()
  ```

- 作用：封装，数据隐藏，外部无法访问嵌套函数。

  ​			贯彻DRY- Don’t Repeat Yourself原则，在函数内部避免重复代码。

  ​			闭包。

  ```python
  def printName(isChinese, name, familyName):
      def inner_print(a, b):
          print("{0} {1}".format(a, b))
      if isChinese:
          inner_print(familyName, name)
      else:
          inner_print(name, familyName)
  printName(True, 'azure', 'rapt')
  printName(False, 'Arch', 'linux')
  ```

## `nonlocal` 关键字：

- nonlocal  用来声明外层的局部变量    global 用来声明全局变量

  ```python
  a = 233
  def external():
      b = 1024
      def internal():
          nonlocal b
          print("internal",b)
          b = 10
          global a
          a = 23333
      internal()
      print("external",b)
  external()
  print("a:",a)
  '''
  internal 1024
  external 10
  a: 23333
  '''
  ```

## LEGB规则：

- Python在查找名称时，按照LEGB规则查找：

  Local - Enclosed - Global - Built in

  Local 函数或者类的方法内部

  Enclosed 嵌套函数(一个函数包裹另一个函数，闭包)

  Global 模块中的全局变量

  Built in 指Python为自己保留的特殊名称

- 如果某个name映射在局部命名空间local中没有找到，就会到闭包作用域进行搜索…依次类推，如果在所有命名空间都没有找到，产生NameError

```python
#str = 'global'
def outer():
    #str = 'outer'
    def inner():
        #str = 'inner'
        print(str)
    inner()
outer()
# <class 'str'>  Built-in
```

