---
title: Python Introduction
categories: Python

---

## 图形化程序设计

- Prepare(Arch):   `sudo pacman -S tk`


```python
import turtle            # 导入turtle模块
turtle.showturtle()      # 显示箭头
turtle.write("raptazure") # 写字符串 
turtle.forward(300)       # 前进300像素
turtle.color("red")       # 画笔颜色改为red
turtle.left(90)           # 箭头左转90度
turtle.goto(0,50)         # 去坐标（0,50）
```

<!-- more -->

-   画一个奥运五环


```python
import turtle
turtle.width(10)
turtle.color("blue")
turtle.circle(50)

turtle.penup()
turtle.goto(120,0)
turtle.pendown()
turtle.color("black")
turtle.circle(50)

turtle.penup()
turtle.goto(240,0)
turtle.pendown()
turtle.color("red")
turtle.circle(50)

turtle.penup()
turtle.goto(60,-50)
turtle.pendown()
turtle.color("yellow")
turtle.circle(50)

turtle.penup()
turtle.goto(180,-50)
turtle.pendown()
turtle.color("green")
turtle.circle(50)
```
## Python程序构成
- 由模块构成，一个模块对应一个python源文件。
- 模块由语句构成，语句是python语句的构造单元，用于创建对象，变量赋值，调用函数，控制语句等。
- 注意缩进 tab ; 使用 \ 行连接符
  ​官方PEP-8代码风格说明：https://www.python.org/dev/peps/pep-0008/

## 对象

​			Python中，一切皆对象。每个对象由标识`id`，类型`type`，值`value` 组成

- 标识用于唯一标识对象，通常对应对象在计算机内存中的地址。使用内置函数id(obj)可以返回对象obj的标识。

- 类型用于表示对象存储的“数据”的类型，类型可以限制对象的取值范围以及可以执行的操作。可以使用type(obj)获得对象的所属类型。

- 值表示对象所存储的数据的信息，使用print(obj)可以直接打印出值。

- 对象的本质是一个内存块，拥有特定的值，支持特定类型的相关操作。

  ```python
  a = 3
  b = 3
  r = id(a)
  s= id(b)
  print(r,s)
  # 140637462492192 140637462492192
  # id：140637462492192
  # type：int
  # value：3
  # 参考 整数缓存机制
  ```

## 引用

- 在Python中，变量也称为：对象的引用。因为变量存储的就是对象的地址。变量通过地址引用了“对象”。变量位于：栈内存   对象位于：堆内存

- Python是动态类型语言，变量不需要显式声明类型。根据变量引用的对象，Python解释器自动确定数据类型。

- Python是强类型语言，每个对象都有数据类型，只支持该类型支持的操作。

## 标识符

- 用于变量，函数，类，模块的名称。
  - 区分大小写

  - 第一个字母必须为字母，下划线

  - 不能使用关键字，如 `if` `or` `while`

  - 以双下划线开头和结尾的名称通常有特殊含义，比如`__init__`是类的构造函数

  - 使用 help() 查看关键字，或者在shell下按F1

-  命名规则：

  - 模块名和包名：全小写，尽量简单，下划线连接  math  os  sys
  - 函数名：全小写，下划线隔开  `my_name`
  - 类名：首字母大写，采用驼峰原则，多个单词时，每个单词首字母大写，其余部分小写。 `MyPhone`   `Phone`
  - 常量名：全大写字母，多个单词用下划线隔开 `MAX_SPEED`

## 变量和简单赋值语句

- 变量声明和赋值用于将一个变量绑定到一个对象上     变量名 = 表达式
- 最简单的表达式是字面量，比如a = 123 。运行过程中，解释器先运行右边的表达式，生成一个代表表达式运算结果的对象，然后再将这个对象地址赋值给左边的变量。
- 删除变量和垃圾回收机制：可通过del语句（从栈中）删除不再使用的变量`del a`。如果对象没有变量引用，就会被垃圾器回收，清理内存空间。

## 链式赋值和系列解包赋值

- 链式赋值用于同一个对象赋值给多个变量  如`x=y=123`

- 系列解包赋值给对应相同个数的变量->个数必须保持一致`a,b,c=4,5,6`

  ```python
  # 系列解包赋值实现变量互换
  a,b = 1,2
  a,b = b,a
  print(a,b)
  ```
- python不支持常量，即没有语法规则限制改变一个常量的值。我们只能约定常量的命名规则，以及在程序的逻辑上不对常量的值作出修改。

## 内置数据类型 & 运算符

- 整形，浮点型，布尔型，字符串型

- `+   -   *   /    //(整数除法)   %   **(幂)`

  ```python
  # 比较运算符可连用  3<a<10
  >>> a = 4
  >>> 3<a<10
  True
  # 位操作
  >>> a = 0b11001
  >>> b = 0b01000
  >>> a
  25
  >>> b
  8
  >>> c = a|b  #按位或
  >>> c
  25
  >>> bin(c)
  '0b11001'
  >>> bin(a&b)
  '0b1000'
  >>> bin(a^b)
  '0b10001'
  # 按位翻转~ x翻转为-(x+1)
  >>> ~a
  -26
  >>> bin(a)
  '0b11001'
  >>> bin(~a)
  '-0b11010'
  >>> a = 3
  >>> a << 1
  6
  >>> a << 3
  24
  >>> a << 2
  12
  >>> a = 8
  >>> a >> 2
  2
  >>> a >> 1
  4
  # Python不支持++和--
  # 相应复合赋值运算符，比如>>=   ^=   **=
  # 优先级：位运算/算术运算>比较运算符>赋值运算符>逻辑运算符
  # 常用小括号
  ```

  

  

- 执行`a += 1`时会找到a进行+1操作生成一个新对象，把新对象地址给a，原来不再被引用的对象会被垃圾回收。

- 使用`divmod()`函数同时得到商和余数  `divmod(13,3)`>>>`(4,1)` -> tuple

- 整数：

  - `0b 0B 二进制` `0o 0O 八进制` `0x 0X 十六进制`  方便位运算

  - 使用`int()`进行类型转换：浮点数舍弃小数部分，布尔值True转为1，False转为0，字符串符合整数格式(浮点数不行)->整数

  - 自动转型：整数和浮点数运算，结果转为浮点数

  - Python3中，int可存储任意大小的整数，取消了long，不存在整数溢出，因此适合科学运算。
  
- 浮点数：
  - 3.14可表示为314e-2或者314E-2
  - 这些数字在内存中也是按照科学计数法存储
  - `float()`强转时生成一个类型为float的新对象
  - `round(value)`可以返回四舍五入的值
  
- 时间的表示：
  - 计算机时间的表示从1970.1.1 00:00:00（unix时间点）开始，以ms为单位进行计时。

  - python中ky通过`time.time()`获得当前时刻，返回值以秒为单位，是1/1000 ms精度的浮点值。

    ```python
    import time
    b = int(time.time())
    totalYears = b/60//60//24//365
    ```
  
- 布尔值：
  - Ture Flase本质还是0与1，可以和数字相加
  - 比较运算符 & 逻辑运算符 `and or not` -> 短路特性
  
- 同一运算符：
  - 同一运算符用于比较两个对象的存储单元，实际比较的是对象是否指向同一个内存地址。运算符`is`判断两个标识符是不是引用同一个对象，`is not`判断两个标识符是否引用不同对象。
  - `is`与`==`区别：is用于判断两个变量引用的对象是否为同一个，比较对象的地址。==用于判断引用变量的值是否相等，默认调用对象的`__eq__()`方法。is 运算比 == 效率高( ==调用了方法)，在变量和None比较时应使用is。
  
- 整数缓存问题：小整数对象在全局解释器范围内被放入缓存以重复使用。
  - Python仅仅对比较小(范围为[-5, 256])的整数对象进行缓存(使用同一个对象而不建立新对象)。需要注意的是，这仅仅是在**命令行**中执行，而在Pycharm，VS Code或者保存为文件执行，结果是不一样的，这是因为解释器进行了一部分优化([-5, inf])。
  
  ```python
  # IDLE
  >>> c=256
  >>> d=256
  >>> c is d
  True
  >>> e =257
  >>> f = 257
  >>> e is f
  False
  ```
## 一个小练习

​		任务：定义多点坐标，绘出折线，计算起始点和终止点距离。

```python
import turtle
import math
# 定义多个点的坐标
x1,y1 = 100,100
x2,y2 = 100,-100
x3,y3 = -100,-100
x4,y4 = -100,100
#绘制折线
turtle.penup()
turtle.goto(x1,y1)
turtle.pendown()
turtle.goto(x2,y2)
turtle.goto(x3,y3)
turtle.goto(x4,y4)
#计算距离
distance = math.sqrt((x1-x4)**2+(y1-y4)**2)
turtle.write(distance)
```

。

