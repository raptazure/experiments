---
title: Python OOP
categories: Python
---
# 面向对象编程

## 简介
​		面向对象编程将数据和操作数据相关方法封装到对象中，组织代码和数据的方式更加接近人的思维，从而大大提高编程效率。更加关注软件中对象中的关系，是一种设计者思维，适合编写大规模程序。面向对象离不开面向过程，二者相辅相成，宏观使用面向对象把握，微观处理仍是面向过程。遇到复杂问题，面向对象先从问题中找名词(面向过程更多的是找动词)，确定这些名词哪些可以作为类，再根据问题需求确定类的属性和方法，确定类之间的关系。

<!--more-->
## 对象

​		将不同类型的数据，方法(函数)放在一起 -> 对象

```python
class Student: # 类名一般首字母大写，采用驼峰规则
    compony = "233"    # 类属性
    count = 0          # 类属性
    def __init__(self, name, score): #self必须位于第一个参数 
        self.name = name      # 实例属性
        self.score = score
        Student.count = Student.count + 1
    def say_score(self):      # 实例方法
        print("my compony is 233")
```

## 类的定义

- 通过类定义数据类型的属性(变量)和方法(行为)，类将行为和状态打包在一起。从一个类创建对象时，每个对象会共享这个类的行为(类中定义的方法)，但会有自己的属性(不共享状态)，更具体一些：“方法代码共享，属性数据不共享”。

- Python中一切皆对象，类也叫做类对象，类的实例也叫做实例对象。

  ```python
  class Student: # 类名一般首字母大写，采用驼峰规则
      compony = "233"    # 类属性
      def __init__(self, name, score):  # self必须位于第一个参数 
          self.name = name      # 实例属性
          self.score = score
      def say_score(self):      # 实例方法
          print(f"{self.name}'s score is {self.score}.")
  
  s1 = Student("rapt", 18) # 通过类名()调用构造函数
  s1.say_score()  #s1是实例对象，自动调用__init()__方法
  ```

## 构造函数`__init__()`

- 类是抽象的，也称为“对象的模板”，通过类这个模板创建类的实例对象，然后才能使用类定义的功能。

- Python对象包含如下部分：id，type，value (attribute, method)

- 创建对象需要定义构造函数`__init__()`方法，构造方法用于执行“实例对象的初始化工作”，即对象创建后，初始化当前对象的相关属性，无返回值。

- `__inti__`名称固定，第一个参数必须为`self`，self就是指刚创建好的实例对象。构造函数通常用来初始化实例对象的实例属性。

- 通过“类名(参数列表)”来调用构造函数，将创建好的对象返回给相应的变量。比如`s1 = Student(‘rapt’, 18)`

- `__init__()`方法：初始化创建好的对象(给实例属性赋值)

  `__new()__`方法：用于创建对象，但一般无需重定义该方法

- Python中的self相当于C++中的self指针，Java和C#中的this关键字，Python中self必须为构造函数的第一个参数，名字可以任意修改。但一般遵守惯例，都叫做self。

## 实例属性

- 从属于实例对象的属性，也称为“实例变量”
- 一般在`__init__()`方法中通过`self.实例属性名 = 初始值`定义
- 在本类的其他实例方法中，也是通过self进行访问 `self.实例属性名`
- 创建实例对象后，通过实例对象访问：
  - `obj01 = 类名()`  创建对象，调用`__init__()`初始化属性
  - `obj01.实例属性名 = 值` 给已有属性赋值，也可以新加属性

## 实例方法

- 从属于对象的方法，定义格式：

  ```python
  def 方法名(self, [形参列表]):
      函数体
  '''
  方法的调用格式如下：
  	对象：方法名([实参列表])
  '''
  ```

- 定义实例方法时，第一个参数必须为self(指当前的实例对象)

- 调用实例方法时，self由解释器自动传参

  ```python
  # 实例对象的方法调用本质：
  a = Student()
  a.say_score()
  # 解释器翻译为 -> Stduent.say_score(a)
  ```

- 其他操作：
  - `dir(obj)` 获得对象的所有属性，方法
  - `obj.__dict__()` 对象(定义)的属性字典
  - `pass` 空语句
  - `isinstance(对象, 类型)` 判断‘对象’是不是‘指定类型’

## 类对象

- 解释器执行class语句时就会创建一个类对象

  ```python
  class Student:
      pass
  
  print(type(Student))
  print(id(Student))
  Stu2 = Student
  s1 = Stu2()
  print(s1)
  '''
  <class 'type'>
  94630656980368
  <__main__.Student object at 0x7f25bddc1ad0>
  '''
  ```
## 类属性和类方法
- 类属性是从属于“类对象”的属性，可以被所有实例对象共享

  ```python
  class Student:
      company = 'microsoft'
      count = 0
      def __init__(self, name, score):
          self.name = name
          self.score = score
          Student.count += 1
      def say_score(self):
          print("my company is:", Student.company)
          print(self.name, "score is", self.score)
  
  s1 = Student("rapt", 100)
  s1.say_score()
  
  s2 = Student("r", 99)
  s2.say_score()
  
  print("created {0} objects in total".format(Student.count))
  '''
  my company is: microsoft
  rapt score is 100
  my company is: microsoft
  r score is 99
  created 2 objects in total
  '''
  ```
  
- 类方法是从属于“类对象”的方法，通过装饰器`@classmethod`来定义

  - 定义时装饰器必须位于方法上面一行

  - 要在第一个位置写上cls (cls指“类对象”本身)

  - 类方法中访问实例属性会导致错误 -> 还不能调self

  - 子类继承父类方法时，传入cls是子类对象，而非父类对象

```python
class Student:
    company = 'radhat'  # 类属性
    @classmethod
    def printCompany(cls):
        print(cls.company)

Student.printCompany()
```

- 静态方法：Python中允许定义与”类对象“无关的方法，称为静态方法

  - 静态方法和在模块中定义普通函数没有区别，只是静态方法放到了类的命名空间里面，需要通过类调用。

  - 通过装饰器`@staticmethod`定义

  - 静态方法中访问实例属性和实例方法会导致错误

```python
class Student:
    company = 'radhat'  # 类属性
    @staticmethod
    def add(a, b):
        print(f"{a} + {b} = {a + b}")
        return a + b

Student.add(4, 5)
```
## 析构函数和垃圾回收机制

- `__del__()`称为析构方法，用于实现对象被销毁时所需的操作，比如：释放对象占用的资源，例如：打开的文件资源，网络连接等。

- Python实现自动的垃圾回收，当对象没有被引用时(引用计数为0)，由垃圾回收器调用`__del__()`方法。

- 也可以通过del语句删除对象，从而调用`__del__()`方法。

- 系统会自动提供`__del__()`方法，一般不需要自定义析构方法。

```python
class Person:
    def __del__(self):
        print("delete object: {0}".format(self))

p1 = Person()
p2 = Person()
del p2
print("the end")
# 之后销毁p1
'''
delete object: <__main__.Person object at 0x7f781ef1d990>
the end
delete object: <__main__.Person object at 0x7f781ef1d910>
'''
```

## `__call__`与可调用对象

- 定义了`__call__`方法的对象，称为可调用对象，即该对象可以像函数一样被调用。

  ```python
  class SalaryAccount:
      def __call__(self, salary):
          print("calculating salary...")
          yearSalary = salary * 12
          daySalary = salary // 22.5
          hourSalary = daySalary // 8
          return dict(yearSalary = yearSalary, monthSalary = salary, daySalary = daySalary, hourSalary = hourSalary)
  
  
  s = SalaryAccount()
  print(s(30000))
  ```

## 方法没有重载

- 在其他语言中，可以定义多个重名的方法，只要保证方法签名(方法名，参数数量。参数类型)唯一即可。Python中，方法的参数没有类型(调用时确定参数类型)，参数的数量也可由可变参数控制。Python中定义一个方法有多种调用方式，相当于实现了其他语言中的方法重载。

- 如果在类体中定义多个重名方法，只有最后一个方法有效

- 方法的动态性：Python是动态语言，可以动态地为类添加新的方法，或者动态地修改类的已有方法。

## 私有属性和私有方法(实现封装)
- Python对于类的成员没有严格的访问控制限制，这与其他面向对象语言有区别
- 通常约定，两个下划线开头的属性是私有的(private)，其他为公共的(public)
- 类内部可以访问私有属性(方法)
- 类外部可通过`__类名__私有属性(方法)名`访问私有属性(方法)
- 方法本质上也是属性，只不过可以通过()执行，所以私有方法和公有方法使用也类似
```python
class Employee:
    __company = 'redhat'

    def __init__(self, name, age):
        self.name = name
        self.__age = age

    def __work(self):
        print("work hard233")
        print(f"age:{self.__age}")

e = Employee('azure', 18)
print(e.name)
print(e._Employee__age)
print(dir(e))
e._Employee__work()
print(Employee._Employee__company)
'''
azure
18
['_Employee__age', '_Employee__company', '_Employee__work', '__class__', '__delattr__', '__dict__', '__dir__', '__doc__', '__eq__', '__format__', '__ge__', '__getattribute__', '__gt__', '__hash__', '__init__', '__init_subclass__', '__le__', '__lt__', '__module__', '__ne__', '__new__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasshook__', '__weakref__', 'name']
work hard233
age:18
redhat
'''
```

## `@property`装饰器

- 可以将一个方法的调用格式变成“属性调用”

```python
# 不使用装饰器
class Employee:
    
    def __init__(self, name, salary):
        self.__name = name
        self.__salary = salary
    
    def get_salary(self):
        return self.__salary
    
    def set_salary(self, salary):
        if 1000 < salary <50000:
            self.__salary = salary
        else:
            print("input error!")
   
emp1 = Employee('rapt', 10000)
print(emp1.get_salary())
emp1.set_salary(-200)
print(emp1.get_salary())
'''
10000
input error!
10000
'''
```

```python
# 使用@property装饰器代替get和set方法
class Employee:
    def __init__(self, name, salary):
        self.__name = name
        self.__salary = salary
    
    @property
    def salary(self):
        return self.__salary
    
    @salary.setter
    def salary(self, salary):
        if 1000 < salary <50000:
            self.__salary = salary
        else:
            print("input error!")

emp1 = Employee('rapt', 10000)
print(emp1.salary)
emp1.salary = -200
print(emp1.salary)
```

## 面向对象三大特征

- 封装(隐藏)：隐藏对象的属性和实现细节，只对外提供必要的方法，相当于“将细节封装起来”，只对外暴露“相关调用方法”。Python没有严格的语法级别的“访问控制符”。通过“私有属性”和“私有方法”实现“封装”。
- 继承：让子类拥有父类的特征，提高代码重用性。从设计上是一种增量进化，原有父类设计不变的情况下，可增加新的功能，或者改进已有算法。
- 多态：指同一个方法调用由于对象不同会产生不同行为。

## 继承

- 如果一个新类(子类/派生类)继承自一个设计好的类(父类/基类)，就直接具备了已有类的特征。

- Python支持多重继承，一个子类可以继承多个父类。如果在类定义中没有指定父类，则默认父类是object类，也就是说，object是所有类的父类，里面定义了一些所有类共有的默认实现，比如`__new__()`

- 定义子类时，必须在其构造函数中调用父类的构造函数，格式为`父类.__init__(self, 参数列表)`

  ```python
  class Person:
      def __init__(self, name, age):
          self.name = name
          self.age = age
  
      def say_age(self):
          print(self.name, "'s age is:", self.age)
  
  class Student(Person):
      def __init__(self, name, age, score):
          # 必须显式调用父类初始化方法，不然解释器不含调用
          Person.__init__(self, name, age)
          self.score = score
  # Student -> Person -> object类
  print(Student.mro())
  s = Student('rapt', 18, 100)
  s.say_age()
  print(s.age)
  '''
  [<class '__main__.Student'>, <class '__main__.Person'>, <class 'object'>]
  rapt 's age is: 18
  18
  '''
  ```

  ```python
  class Person:
      def __init__(self, name, age):
          self.name = name
          self.__age = age
  
      def say_age(self):
          print(self.name, "'s age is:", self.__age)
  
  class Student(Person):
      def __init__(self, name, age, score):
          # 必须显式调用父类初始化方法，不然解释器不含调用
          Person.__init__(self, name, age)
          self.score = score
  # Student -> Person -> object类
  print(Student.mro())
  s = Student('rapt', 18, 100)
  s.say_age()
  # print(s.age)
  print(dir(s))
  print(s._Person__age)
  '''
  [<class '__main__.Student'>, <class '__main__.Person'>, <class 'object'>]
  rapt 's age is: 18
  ['_Person__age', '__class__', '__delattr__', '__dict__', '__dir__', '__doc__', '__eq__', '__format__', '__ge__', '__getattribute__', '__gt__', '__hash__', '__init__', '__init_subclass__', '__le__', '__lt__', '__module__', '__ne__', '__new__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasshook__', '__weakref__', 'name', 'say_age', 'score']
  18
  '''
  ```

## 类成员的继承和重写

- 成员继承：子类继承了父类除构造方法之外的所有成员

- 方法重写：子类可以重新定义父类中的方法，会覆盖(重写)父类中的方法

  ```python
  class Person:
      def __init__(self, name, age):
          self.name = name
      
      def introduce(self):
          print(f"my name is {self.name}")
  
  class Student(Person):
      def __init__(self, name, age, score):
          Person.__init__(self, name, age)
          self.score = score
      def introduce(self):
          '''重写父类方法'''
          print(f"hello, my name is {self.name}")
  s = Student('rapt', 18, 100)
  s.introduce()
  '''
  hello, my name is rapt
  '''
  ```

## `object`根类

- object是所有类的父类，所有类都有object的属性和方法，利用`dir()`查看对象的所有属性来观察object的结构

  ```python
  class Person:
      def __init__(self, name, age):
          self.name = name
          self.age = age
      
      def say_age(self):
          print(f"my age is {self.age}")
      
  obj = object()
  print(dir(obj))
  s = Person('zure', 18)
  print(dir(s))
  '''
  ['__class__', '__delattr__', '__dir__', '__doc__', '__eq__', '__format__', '__ge__', '__getattribute__', '__gt__', '__hash__', '__init__', '__init_subclass__', '__le__', '__lt__', '__ne__', '__new__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasshook__']
  ['__class__', '__delattr__', '__dict__', '__dir__', '__doc__', '__eq__', '__format__', '__ge__', '__getattribute__', '__gt__', '__hash__', '__init__', '__init_subclass__', '__le__', '__lt__', '__module__', '__ne__', '__new__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasshook__', '__weakref__', 'age', 'name', 'say_age']
  '''
  ```

- 从上面的测试发现，Person对象增加了6个属性：`__dict__, __module__, __weakref__, age, name, say_age`，`say_name`虽然是方法，但实际上也是属性，属性的类型为method

## 重写`__str__()`方法

```python
# 重写之前
class Person:   # 默认继承object类
    def __init__(self, name):
        self.name = name

p  = Person('orz')
print(p)
'''
<__main__.Person object at 0x7fbdd8b92b10>
'''
```

```python
# 重写之后
class Person:   # 默认继承object类
    def __init__(self, name):
        self.name = name
    
    def __str__(self):
        return "real name is {}".format(self.name)

p  = Person('orz')
print(p)
'''
real name is orz
'''
```

## 多重继承

- Python支持多重继承，一个子类可以有多个直接父类，这样就可以具备多个父类的特点，但这样会使“类的结构层次”变得复杂，尽量避免使用。

```python
class A:
    def aa(self):
        print("A")
class B:
    def bb(self):
        print("B")
class C(B, A):
    def cc(self):
        print("C")

c = C()
c.cc()
c.bb()
c.aa()
```

## `mro()`

- Python支持多继承，如果父类中有相同名字的方法，在子类没有指定父类名时，解释器将“从左往右”按顺序搜索。

- MRO(Method Resolution Order)：方法解析顺序(采用广度优先算法)。通过类的方法`mro()`或者类的属性`__mro__()`可以输出这个类的继承层次结构。方法解析顺序也是按照这个“类的层次结构”寻找的。

  ```python
  class A:
      def aa(self):
          print("A")
  class B:
      def bb(self):
          print("B")
  class C(B, A):
      def cc(self):
          print("C")
  
  print(C.mro())
  '''
  [<class '__main__.C'>, <class '__main__.B'>, <class '__main__.A'>, <class 'object'>]
  '''
  ```

## `super()`获得父类定义

- 代表父类的定义，而不是父类的对象

  ```python
  class A:
      def say(self):
          print("A:", self)
  class B(A):
      def say(self):
          # A.say(self)
          super().say()
          print("B:", self)
  
  B().say()
  '''
  A: <__main__.B object at 0x7f3611315950>
  B: <__main__.B object at 0x7f3611315950>
  '''
  ```

## 多态

- 同一方法调用由于对象不同可能产生不同行为。多态是指方法的多态，属性没有多态。多态的存在有两个必要条件：继承，方法重写。

  ```python
  class Man:
      def eat(self):
          print("need more food!")
  
  class Chinese(Man):
      def eat(self):
          print("chopsticks!")
  
  class English(Man):
      def eat(self):
          print("fork and knife!")
  
  def manEat(m):
      if isinstance(m, Man):
          m.eat()
      else:
          print("no food at all!")
  
  manEat(Chinese())
  manEat(English())
  '''
  chopsticks!
  fork and knife!
  '''
  ```

## 特殊方法与运算符重载

- Python运算符实际上是调用对象的特殊方法实现的。

  ```python
  a = 20
  b = 30
  c = a + b
  d = a.__add__(b)
  print("c =",c)
  print("d =",d)
  '''
  c = 50
  d = 50
  '''
  ```

- 常用的特殊方法：

  |        方法         |    说明    |          例子           |
  | :-----------------: | :--------: | :---------------------: |
  |     `__init__`      |  构造方法  | 对象创建： p = Person() |
  |      `__del__`      |  析构方法  |        对象回收         |
  | `__repr__, __str__` | 打印，转换 |        print(a)         |
  |     `__call__`      |  函数调用  |           a()           |
  |    `__getattr__`    |  点号运算  |          a.xxx          |
  |    `__setattr__`    |  属性赋值  |      a.xxx = value      |
  |    `__getitem__`    |  索引运算  |         a[key]          |
  |    `__setitem__`    |  索引赋值  |     a[key] = value      |
  |      `__len__`      |    长度    |         len(a)          |

- 每个运算符实际上都对应了相应的方法：

  |   运算符    |                   特殊方法                    |
  | :---------: | :-------------------------------------------: |
  |      +      |                   `__add__`                   |
  |      -      |                   `__sub__`                   |
  | `<  <=  ==` |           `__lt__, __le__, __eq__`            |
  | `>  >=  !=` |           `__gt__, __ge__, __ne__`            |
  |  \|  ^  &   |           `__or__, __xor__, _and__`           |
  |   <<  >>    |           `__lshift__, __rshift__`            |
  | *  /  %  // | `__mul__, __truediv__, __mod__, __floordiv__` |
  |     **      |                   `__pow__`                   |

  ```python
  class Person:
      def __init__(self, name):
          self.name = name
      
      def __add__(self, other):
          if isinstance(other, Person):
              return "{0}--{1}".format(self.name, other.name)
          else:
              return "can not be added!"
      
      def __mul__(self, other):
          if isinstance(other, int):
              return self.name * other
          else:
              return "can not be multiplied!"
  
  p1 = Person("rapt")
  p2 = Person("azure")
  
  x = p1 + p2
  print(x)
  print(p1 * 3)
  '''
  rapt--azure
  raptraptrapt
  '''
  ```

## 特殊属性

|         特殊方法         |         含义         |
| :----------------------: | :------------------: |
|      `obj.__dict__`      |    对象的属性字典    |
|     `obj.__class__`      |     对象所属的类     |
|    `class.__bases__`     | 类的基类元组(多继承) |
|     `class.__base__`     |       类的基类       |
|     `class.__mro__`      |      类层次结构      |
| `class.__subclasses__()` |       子类列表       |

```python
class A:
    pass
class B:
    pass
class C(B, A):
    def __init__(self, num):
        self.num = num
    def cc(self):
        print("cc")

c = C(3)
print(dir(c))
print(c.__dict__)
print(c.__class__)
print(C.__bases__)
print(C.__base__)
print(C.__mro__)
print(A.__subclasses__)
'''
['__class__', '__delattr__', '__dict__', '__dir__', '__doc__', '__eq__', '__format__', '__ge__', '__getattribute__', '__gt__', '__hash__', '__init__', '__init_subclass__', '__le__', '__lt__', '__module__', '__ne__', '__new__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasshook__', '__weakref__', 'cc', 'num']
{'num': 3}
<class '__main__.C'>
(<class '__main__.B'>, <class '__main__.A'>)
<class '__main__.B'>
(<class '__main__.C'>, <class '__main__.B'>, <class '__main__.A'>, <class 'object'>)
<built-in method __subclasses__ of type object at 0x55687d191590>
'''
```

## 对象的浅拷贝和深拷贝

- 变量赋值：只是形成两个变量，实际还是指向同一个对象
- 浅拷贝：Python中拷贝一般都是浅拷贝。拷贝时，对象包含的子对象内容不拷贝，因此源对象和拷贝对象会引用同一个子对象。
- 深拷贝：使用copy模块的deepcopy函数，递归拷贝对象中包含的子对象，源对象和拷贝对象所有的子对象也不同。

```python
import copy
class MobilePhone:
    def __init__(self, cpu, screen):
        self.cpu = cpu
        self.screen = screen

class CPU:
    def calc(self):
        print("5 =",3 + 2)
        print("cpu object:",self)

class Screen:
    def show(self):
        print("nice picture")
        print("screen object", self)

# 测试变量赋值
c1 = CPU()
c2 = c1
print(c1)
print(c2)

# 测试浅复制
s1 = Screen()
m1 = MobilePhone(c1, s1)
m2 = copy.copy(m1)
print(m1, m1.cpu, m1.screen)
print(m2, m2.cpu, m2.screen)

# 测试深复制
m3 = copy.deepcopy(m1)
print(m1, m1.cpu, m1.screen)
print(m2, m2.cpu, m2.screen)
'''
<__main__.CPU object at 0x7ffb8df27c10>
<__main__.CPU object at 0x7ffb8df27c10>
<__main__.MobilePhone object at 0x7ffb8df27e50> <__main__.CPU object at 0x7ffb8df27c10> <__main__.Screen object at 0x7ffb8df27d10>
<__main__.MobilePhone object at 0x7ffb8df2c6d0> <__main__.CPU object at 0x7ffb8df27c10> <__main__.Screen object at 0x7ffb8df27d10>
<__main__.MobilePhone object at 0x7ffb8df27e50> <__main__.CPU object at 0x7ffb8df27c10> <__main__.Screen object at 0x7ffb8df27d10>
<__main__.MobilePhone object at 0x7ffb8df2c6d0> <__main__.CPU object at 0x7ffb8df27c10> <__main__.Screen object at 0x7ffb8df27d10>
'''
```

## 组合

- “is-a”关系，可以使用“继承”，从而实现子类拥有父类的方法和属性，如：狗是动物

- “has-a”关系，可以使用”组合“，也能实现一个类拥有另一个类的方法和属性，如：手机有CPU

  ```python
  # 使用继承实现代码复用
  class A1:
      def say_a1(self):
          print("a1")
  class B1(A1):
      pass
  
  b1 = B1()
  b1.say_a1()
  
  # 使用组合实现代码复用(效果相同)
  class A2:
      def say_a2(self):
          print("a2")
  class B2:
      def __init__(self, a):
          self.a = a
  
  b2 = B2(A2())
  b2.a.say_a2()
  '''
  a1
  a2
  '''
  ```

  ```python
  # 测试has-a关系，使用组合
  class MobilePhone:
      def __init__(self, cpu, screen):
          self.cpu = cpu
          self.screen = screen
  
  class CPU:
      def calc(self):
          print("5 =",3 + 2)
          print("cpu object:",self)
  
  class Screen:
      def show(self):
          print("nice picture")
          print("screen object", self)
  
  m = MobilePhone(CPU(), Screen())
  m.cpu.calc()
  m.screen.show()
  '''
  5 = 5
  cpu object: <__main__.CPU object at 0x7fab84874a90>
  nice picture
  screen object <__main__.Screen object at 0x7fab84874b90>
  '''
  ```

## 设计模式

- 设计模式是面向对象语言特有的内容，是我们在面临某一类问题时候固定的做法，设计模式有很多种，比较流行的是：GOF(Group of Four)23种设计模式。

- 工厂模式：实现了创建者与调用者的分离，使用专门的工厂类将选择实现类、创建对象进行统一的管理和控制

  ```python
  class CarFactory:
      def create_car(self, brand):
          if brand == 'Benz':
              return Benz()
          elif brand == 'BMW':
              return BWM()
          elif brand == 'Lamboghini':
              return Lamboghini()
          else:
              return "unknown"
  
  class Benz:
      pass
  
  class BWM:
      pass
  
  class Lamboghini:
      pass
  
  factory = CarFactory()
  c1 = factory.create_car('Benz')
  c2 = factory.create_car('BMW')
  print(c1)
  print(c2)
  '''
  <__main__.Benz object at 0x7f334f5b1ad0>
  <__main__.BWM object at 0x7f334f5b1b10>
  '''
  ```

- 单例模式(Singleton Pattern)：核心作用是确保一个类只有一个实例，并且提供一个访问该实例的全局访问点。单例模式只生成一个实例对象，减少了对系统资源的开销。当一个对象的产生需要比较多的资源，如读取配置文件，产生其他依赖对象时，可以产生一个“单例对象”，然后永久驻留在内存中，从而降低开销。

```python
class MySingleton:
    __obj = None
    __init__flag = True
    def __new__(cls, *args, **kwargs):
        if cls.__obj is None:
            cls.__obj = object.__new__(cls)
        return cls.__obj
    
    def __init__(self, name):
        if MySingleton.__init__flag:
            print("Initializing...")
            self.name = name
            MySingleton.__init__flag = False

a = MySingleton("a")
b  = MySingleton("b")
c = MySingleton("c")
print(a)
print(b)
print(c)
'''
Initializing...
<__main__.MySingleton object at 0x7f6f0dc1c890>
<__main__.MySingleton object at 0x7f6f0dc1c890>
<__main__.MySingleton object at 0x7f6f0dc1c890>
'''
```

- 工厂+单例：

  ```python
  class CarFactory:
      __obj = None
      __init__flag = True
      
      def create_car(self, brand):
          if brand == 'Benz':
              return Benz()
          elif brand == 'BMW':
              return BWM()
          elif brand == 'Lamboghini':
              return Lamboghini()
          else:
              return "unknown"
      
      def __new__(cls, *args, **kwargs):
          if cls.__obj is None:
              cls.__obj = object.__new__(cls)
          return cls.__obj
      
      def __init__(self):
          if CarFactory.__init__flag:
              print("Initializing CarFactory...")
              CarFactory.__init__flag = False
  
  class Benz:
      pass
  
  class BWM:
      pass
  
  class Lamboghini:
      pass
  
  factory = CarFactory()
  c1 = factory.create_car('Benz')
  c2 = factory.create_car('BMW')
  print(c1)
  print(c2)
  
  factory2 = CarFactory()
  print(factory)
  print(factory2)
  '''
  Initializing CarFactory...
  <__main__.Benz object at 0x7f62ca5dfc10>
  <__main__.BWM object at 0x7f62ca5dfc50>
  <__main__.CarFactory object at 0x7f62ca5dfbd0>
  <__main__.CarFactory object at 0x7f62ca5dfbd0>
  '''
  ```