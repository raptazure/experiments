**复习笔记** by Raptazure

- [x] 继承

- [x] 多态

- [x] 类的成员函数

- [x] 构造函数

- [x] 赋值函数

- [x] 析构函数

- [x] 虚函数

- [x] 纯虚函数与抽象类

- [x] 静态成员函数

- [x] 友元函数

- [x] 内联函数

- [x] 模版

- [x] 异常处理


## Introduction

&nbsp; &nbsp; &nbsp; 看了看模拟题，应该是主要考察 `OOP` 相关的概念，`STL` 顶多冒出来个 `vector`（考前几分钟网上看看 API 会用就行，各种模版容器还是算法题用的多hhh），其他的，也就再考考引用？别的估计也涉及不到了。所以这里就简单总结一下 `OOP` 相关的概念，不过，其实 C++ 是我所有写过的语言中最不熟悉的，幸好 JS 中理解的面向对象可以帮下忙。

## Object Oriented Programming

类和对象：抽象和具体

### 继承与多态：

概念很简单，直接举🌰

- 先是继承：

```cpp
// C++ program to demonstrate implementation of Inheritance 
   
#include <bits/stdc++.h> // 一只万能头函数，考试还是不要写这个 = =
using namespace std; 
  
//Base class 
class Parent 
{ 
    public: 
      int id_p; 
}; 
   
// Sub class inheriting from Base Class(Parent) 
class Child : public Parent 
{ 
    public: 
      int id_c; 
}; 
  
//main function 
int main()  
{ 
       
    Child obj1; 
        
    // An object of class child has all data members 
    // and member functions of class parent 
    obj1.id_c = 7; 
    obj1.id_p = 91; 
    cout << "Child id is " <<  obj1.id_c << endl; 
    cout << "Parent id is " <<  obj1.id_p << endl; 
        
    return 0; 
}  
```

关于 `public, private, protected`：

```cpp
class A  
{ 
public: 
    int x; 
protected: 
    int y; 
private: 
    int z; 
}; 
  
class B : public A 
{ 
    // x is public 
    // y is protected 
    // z is not accessible from B 
}; 
  
class C : protected A 
{ 
    // x is protected 
    // y is protected 
    // z is not accessible from C 
}; 
  
class D : private A    // 'private' is default for classes 
{ 
    // x is private 
    // y is private 
    // z is not accessible from D 
}; 
```

一个更复杂的继承关系(混合) => 可以尝试画出类似继承树一样的东东帮助理解

```cpp
// C++ program for Hybrid Inheritance 
  
#include <iostream> 
using namespace std; 
  
// base class  
class Vehicle  
{ 
  public: 
    Vehicle() 
    { 
      cout << "This is a Vehicle" << endl; 
    } 
}; 
  
//base class 
class Fare 
{ 
    public: 
    Fare() 
    { 
        cout<<"Fare of Vehicle\n"; 
    } 
}; 
  
// first sub class  
class Car: public Vehicle 
{ 
  
}; 
  
// second sub class 
class Bus: public Vehicle, public Fare 
{ 
      
}; 
  
// main function 
int main() 
{    
    // creating object of sub class will 
    // invoke the constructor of base class 
    Bus obj2; 
    return 0; 
} 
// This is a Vehicle
// Fare of Vehicle
```

- 再来多态：

1. 编译时多态（函数重载和运算符重载）

> **[Function Overloading](https://www.geeksforgeeks.org/function-overloading-c/)**: When there are multiple functions with same name but different parameters then these functions are said to be **overloaded**. Functions can be overloaded by **change in number of arguments** or/and **change in type of arguments**.

```cpp
#include <bits/stdc++.h> 
  
using namespace std; 
class Geeks 
{ 
    public: 
      
    // function with 1 int parameter 
    void func(int x) 
    { 
        cout << "value of x is " << x << endl; 
    } 
      
    // function with same name but 1 double parameter 
    void func(double x) 
    { 
        cout << "value of x is " << x << endl; 
    } 
      
    // function with same name and 2 int parameters 
    void func(int x, int y) 
    { 
        cout << "value of x and y is " << x << ", " << y << endl; 
    } 
}; 
  
int main() { 
      
    Geeks obj1; 
      
    // Which function is called will depend on the parameters passed 
    // The first 'func' is called  
    obj1.func(7); 
      
    // The second 'func' is called 
    obj1.func(9.132); 
      
    // The third 'func' is called 
    obj1.func(85,64); 
    return 0; 
}  
// value of x is 7
// value of x is 9.132
// value of x and y is 85, 64
```

> **[Operator Overloading](https://www.geeksforgeeks.org/operator-overloading-c/)**: C++ also provide option to overload operators. For example, we can make the operator (‘+’) for string class to concatenate two strings. We know that this is the addition operator whose task is to add two operands.  So a single operator ‘+’ when placed between integer operands , adds  them and when placed between string operands, concatenates them.

```cpp
#include<iostream> 
using namespace std; 
   
class Complex { 
private: 
    int real, imag; 
public: 
    Complex(int r = 0, int i =0)  {real = r;   imag = i;} 
       
    // This is automatically called when '+' is used with 
    // between two Complex objects 
    Complex operator + (Complex const &obj) { 
         Complex res; 
         res.real = real + obj.real; 
         res.imag = imag + obj.imag; 
         return res; 
    } 
    void print() { cout << real << " + i" << imag << endl; } 
}; 
   
int main() 
{ 
    Complex c1(10, 5), c2(2, 4); 
    Complex c3 = c1 + c2; // An example call to "operator+" 
    c3.print(); 
} 
// 12 + i9
```

2. 运行时多态（函数重写）=> 这块可以结合下面的虚函数来理解

> **[Function overriding](https://www.geeksforgeeks.org/override-keyword-c/)** on the other hand occurs when a derived class has a definition for one  of the member functions of the base class. That base function is said to be **overridden**.

```cpp
#include <bits/stdc++.h> 
using namespace std; 
  
class base 
{ 
public: 
    virtual void print () 
    { cout<< "print base class" <<endl; } 
   
    void show () 
    { cout<< "show base class" <<endl; } 
}; 
   
class derived:public base 
{ 
public: 
    void print () //print () is already virtual function in derived class, we could also declared as virtual void print () explicitly 
    { cout<< "print derived class" <<endl; } 
   
    void show () 
    { cout<< "show derived class" <<endl; } 
}; 
  
//main function 
int main()  
{ 
    base *bptr; 
    derived d; 
    bptr = &d; 
       
    //virtual function, binded at runtime (Runtime polymorphism) 
    bptr->print();  
       
    // Non-virtual function, binded at compile time 
    bptr->show();  
  
    return 0; 
}  
// print derived class
// show base class
```



### 类的成员函数：

  - 六个默认成员函数：构造函数，拷贝构造函数，析构函数，赋值操作符重载，取地址符重载，`const` 修饰的取地址符操作符重载，比如：
```cpp
class A {

};

// 经过编译器处理

class A {
public:
	A(); // 构造函数
	A(const A& a); // 拷贝构造函数
	~A(); // 析构函数
	A& operator=(const A& a); // 赋值运算符重载
	A* operator &(); // 取地址运算符重载
	const A* operator &() const; // const修饰的取地址运算符重载
};
```
##### 构造函数

> 一个特殊的成员函数，名字与类名相同，没有返回值类型，创建类类型对象的时候，由编译器自动调用，在对象的生命周期内只且调用一次，以保证每个数据成员都有一个合适的初始值。

- 默认构造函数：不需传入任何参数，即使不明确定义任何构造函数，编译器也会自动生成

```cpp
#include <iostream> 
using namespace std; 
  
class construct { 
public: 
    int a, b; 
  
    // Default Constructor 
    construct() 
    { 
        a = 10; 
        b = 20; 
    } 
}; 
  
int main() 
{ 
    // Default constructor called automatically 
    // when the object is created 
    construct c; 
    cout << "a: " << c.a << endl 
         << "b: " << c.b; 
    return 1; 
} // a: 10  b: 20
```

- **有参构造函数:** 除了无参构造函数，构造函数也是可以有参数的，这时需要传入相应的参数来创建对象，用于初始化有不同值的不同对象。

```cpp
#include <iostream> 
using namespace std; 
  
class Point { 
private: 
    int x, y; 
  
public: 
    // Parameterized Constructor 
    Point(int x1, int y1) 
    { 
        x = x1; 
        y = y1; 
    } 
  
    int getX() 
    { 
        return x; 
    } 
    int getY() 
    { 
        return y; 
    } 
}; 
  
int main() 
{ 
    // Constructor called 
    Point p1(10, 15); 
  
    // Access values assigned by constructor 
    cout << "p1.x = " << p1.getX() << ", p1.y = " << p1.getY(); 
  
    return 0; 
} 
```

也可用于重载 `constructor`：

```cpp
#include <iostream> 
using namespace std; 
  
class construct 
{  
  
public: 
    float area;  
      
    // Constructor with no parameters 
    construct() 
    { 
        area = 0; 
    } 
      
    // Constructor with two parameters 
    construct(int a, int b) 
    { 
        area = a * b; 
    } 
      
    void disp() 
    { 
        cout<< area<< endl; 
    } 
}; 
  
int main() 
{ 
    // Constructor Overloading  
    // with two different constructors 
    // of class name 
    construct o; 
    construct o2( 10, 20); 
      
    o.disp(); 
    o2.disp(); 
    return 1; 
} 
```

- 拷贝构造函数：用一个类中的另一个对象来初始化新对象的成员函数

  函数原型：`ClassName (const ClassName &old_obj); `

  ```cpp
  #include<iostream> 
  using namespace std; 
    
  class Point 
  { 
  private: 
      int x, y; 
  public: 
      Point(int x1, int y1) { x = x1; y = y1; } 
    
      // Copy constructor 
      Point(const Point &p2) {x = p2.x; y = p2.y; } 
    
      int getX()            {  return x; } 
      int getY()            {  return y; } 
  }; 
    
  int main() 
  { 
      Point p1(10, 15); // Normal constructor is called here 
      Point p2 = p1; // Copy constructor is called here 
    
      // Let us access values assigned by constructors 
      cout << "p1.x = " << p1.getX() << ", p1.y = " << p1.getY(); 
      cout << "\np2.x = " << p2.getX() << ", p2.y = " << p2.getY(); 
    
      return 0; 
  }
  // p1.x = 10, p1.y = 15
  // p2.x = 10, p2.y = 15 
  ```

关于拷贝复制函数的Q & A：来自 `Geekforgeeks`
  > **啥时候调用？**
  >
  > In C++, a Copy Constructor may be called in following cases:
  >
  > 1. When an object of the class is returned by value.
  >
  > 2. When an object of the class is passed (to a function) by value as an argument.
  >
  > 3. When an object is constructed based on another object of the same class.
  > 4. When the compiler generates a temporary object.
  >
  > **啥时候需要用户自定义拷贝构造函数？**
  >
  >  If we don’t define our own copy constructor, the C++ compiler creates a  default copy constructor for each class which does a member-wise copy  between objects. The compiler created copy constructor works fine in  general.  We need to define our own copy constructor only if an object  has pointers or any runtime allocation of the resource like file handle, a network connection..etc. ***Default constructor does only shallow copy.*** ***Deep copy is possible only with user defined copy constructor.*** In user defined copy constructor, we make sure that pointers (or references) of copied object point to new memory locations.

比如以下代码，比较移除拷贝构造函数后输出的区别：

```cpp
#include<iostream> 
#include<cstring> 
using namespace std; 
  
class String 
{ 
private: 
    char *s; 
    int size; 
public: 
    String(const char *str = NULL); // constructor 
    ~String() { delete [] s;  }// destructor 
    String(const String&); // copy constructor 
    void print() { cout << s << endl; } // Function to print string 
    void change(const char *);  // Function to change 
}; 
  
String::String(const char *str) 
{ 
    size = strlen(str); 
    s = new char[size+1]; 
    strcpy(s, str); 
} 
  
void String::change(const char *str) 
{ 
    delete [] s; 
    size = strlen(str); 
    s = new char[size+1]; 
    strcpy(s, str); 
} 
  
String::String(const String& old_str) 
{ 
    size = old_str.size; 
    s = new char[size+1]; 
    strcpy(s, old_str.s); 
} 
  
int main() 
{ 
    String str1("GeeksQuiz"); 
    String str2 = str1; 
  
    str1.print(); // what is printed ? 
    str2.print(); 
  
    str2.change("GeeksforGeeks"); 
  
    str1.print(); // what is printed now ? 
    str2.print(); 
    return 0; 
} 
// GeeksQuiz
// GeeksQuiz
// GeeksQuiz
// GeeksforGeeks
// 因为是深拷贝，对str2的影响不会使str1改变，而如果删除自定义的拷贝构造函数，使用编译器自动生成的，就会变成浅拷贝，导致结果成为：
// GeeksQuiz
// GeeksQuiz
// GeeksforGeeks
// GeeksforGeeks
```

> **为啥传入的参数必须是引用？**
>
> 因为对象按值传入一个函数的时候会调用拷贝复制函数，所以如果我们对拷贝复制函数按值传参，就会出现拷贝复制函数调拷贝复制函数的无休止的尴尬局面，所以编译器不让那么干。
>
> **为啥传入的参数必须是`const`？**
>
> 一个原因是我们写 C++ 应该在任何能用const的地方都用const，来防止对象被意外修改，但是还有其他原因，看下面的程序：

```cpp
#include<iostream> 
using namespace std; 
  
class Test 
{ 
   /* Class data members */ 
public: 
   Test(Test &t) { /* Copy data members from t*/} 
   Test()        { /* Initialize data members */ } 
}; 
  
Test fun() 
{ 
    cout << "fun() Called\n"; 
    Test t; 
    return t; 
} 
  
int main() 
{ 
    Test t1; 
  	// error
    Test t2 = fun(); 
  	// works fine
		// Test t2;  
		// t2 = fun(); 

    return 0; 
} 
// Compiler Error in line "Test t2 = fun();" 
// fun() 返回一个值，所以编译器创建一个用拷贝构造函数复制到 t2 的临时对象（这个临时对象作为拷贝构造函数的参数），出现编译错误的原因是，编译器创建的临时对象不能被绑定到非常引用上（对编译器创建的临时对象进行改动没有任何意义，因为他们随时都可能消亡）
```

##### 赋值操作符重载 (赋值函数)

编译器也会自动生成，若要自定义，则原因与拷贝构造函数相似，涉及深浅拷贝的问题

```cpp
#include <iostream>
#include <cstring>
using namespace std;

class String
{
public:
  String(const char *str);
  String(const String &other);
  String &operator=(const String &other);
  ~String(void);

private:
  char *m_data;
};

String::String(const char *str)
{
  cout << "自定义构造函数" << endl;
  if (str == NULL)
  {
    m_data = new char[1];
    *m_data = '\0';
  }
  else
  {
    int length = strlen(str);
    m_data = new char[length + 1];
    strcpy(m_data, str);
  }
}

String::String(const String &other)
{
  cout << "自定义拷贝构造函数" << endl;
  int length = strlen(other.m_data);
  m_data = new char[length + 1];
  strcpy(m_data, other.m_data);
}

String &String::operator=(const String &other)
{
  cout << "自定义赋值函数" << endl;

  if (this == &other)
  {
    return *this;
  }
  else
  {
    delete[] m_data;
    int length = strlen(other.m_data);
    m_data = new char[length + 1];
    strcpy(m_data, other.m_data);
    return *this;
  }
}

String::~String(void)
{
  cout << "自定义析构函数" << endl;
  delete[] m_data;
}

int main()
{
  cout << "a(\"abc\")" << endl;
  String a("abc");

  cout << "b(\"cde\")" << endl;
  String b("cde");

  cout << " d = a" << endl;
  String d = a;

  cout << "c(b)" << endl;
  String c(b);

  cout << "c = a" << endl;
  c = a;

  cout << endl;
}
// a("abc")
// 自定义构造函数
// b("cde")
// 自定义构造函数
// d = a
// 自定义拷贝构造函数
// c(b)
// 自定义拷贝构造函数
// c = a
// 自定义赋值函数
//
// 自定义析构函数
// 自定义析构函数
// 自定义析构函数
// 自定义析构函数
//
```

- 取地址符重载和 `const` 修饰的取地址符操作符重载就不写了，感觉不会考，感兴趣可以搜索引擎一下。

  

##### 析构函数

> 析构函数是销毁或删除对象的成员函数，当函数或程序执行完毕，块作用域的局部变量消亡或使用 `delete `操作符的时候会调用 `destructor`。

```cpp
class String 
{ 
private: 
    char *s; 
    int size; 
public: 
    String(char *); // constructor 
    ~String();      // destructor 
}; 
  
String::String(char *c) 
{ 
    size = strlen(c); 
    s = new char[size+1]; 
    strcpy(s,c); 
} 
  
String::~String() 
{ 
    delete []s; 
} 
```

关于析构函数的 Q & A: 来自 `Geekforgeeks`

> **How destructors are different from a normal member function?**
>  Destructors have same name as the class preceded by a tilde (~)
>  Destructors don’t take any argument and don’t return anything
>
> **Can there be more than one destructor in a class?**
>  No, there can only one destructor in a class with classname preceded by ~, no parameters and no return type.
>
> **When do we need to write a user-defined destructor?**
>  If we do not write our own destructor in class, compiler creates a  default destructor for us. The default destructor works fine unless we  have dynamically allocated memory or pointer in class. When a class  contains a pointer to memory allocated in class, we should write a  destructor to release memory before the class instance is destroyed.  This must be done to avoid memory leak.
>
> **Can a destructor be virtual?**
>  Yes, In fact, it is always a good idea to make destructors virtual in base class when we have a virtual function.  

不过，在介绍 Virtual Destructor 之前，我们先来看看啥叫 `Virtual Function` 和 `Pure Virtual Function` 以及随之而来要提到的抽象类：

> - 虚函数是在基类中声明的成员函数，并由派生类重写。当使用指针或对基类的引用来引用派生类对象时，可以为该对象调用虚函数并执行该派生类版本的函数。它们主要用于实现[运行时多态](https://translate.googleusercontent.com/translate_c?depth=1&hl=zh-CN&pto=aue&rurl=translate.google.com&sl=auto&sp=nmt4&u=https://www.geeksforgeeks.org/polymorphism-in-c/&usg=ALkJrhh_ZZou-6x9NCdTe5t7z5LIxNBJPQ)，函数调用的解析在运行时完成。
> - 虚函数不能是静态的，也不能是另一个类的友元函数。应该使用基类类型的指针或引用来访问虚拟函数，以实现运行时多态。虚拟函数的原型在基类和派生类中都应相同。它们始终在基类中定义，而在派生类中被覆盖。并非强制派生类重写（或重新定义虚拟函数），在这种情况下，将使用函数的基类版本。一个类可能具有虚拟析构函数，但不能具有虚拟构造函数。

`````cpp
#include <iostream> 
using namespace std; 
  
class base { 
public: 
    virtual void print() 
    { 
        cout << "print base class" << endl; 
    } 
  
    void show() 
    { 
        cout << "show base class" << endl; 
    } 
}; 
  
class derived : public base { 
public: 
    void print() 
    { 
        cout << "print derived class" << endl; 
    } 
  
    void show() 
    { 
        cout << "show derived class" << endl; 
    } 
}; 
  
int main() 
{ 
    base* bptr; 
    derived d; 
    bptr = &d; 
  
    // virtual function, binded at runtime 
    bptr->print(); 
  
    // Non-virtual function, binded at compile time 
    bptr->show(); 
} 

// print derived class
// show base class

// 说明：运行时多态只能通过基类类型的指针（或引用）来实现。同样，基类指针可以指向基类的对象以及派生类的对象。在上面的代码中，基类指针“bptr”包含派生类的对象“d”的地址。

// 后期绑定（运行时）是根据指针的内容（即指针指向的位置）完成的，而早期绑定（编译时）是根据指针的类型完成的，因为print()函数是用virtual关键字声明的，因此将在运行时绑定（输出是打印派生类，因为指针指向派生类的对象），而show()是非虚拟的，因此它将在编译时绑定（输出是显示基类，因为指针是基类的）键入。

// 注意：如果我们在基类中创建了虚函数，并且在派生类中将其重写，则在派生类中不需要virtual关键字，则函数将自动视为派生类中的虚函数。
`````

> - 有时无法在基类中提供所有函数的实现，因为我们不知道实现，这样的类称为抽象类。举个🌰，让 Shape 作为基类，我们不能提供 Shape 里 `draw()` 函数的实现，但是知道每一个 Shape 的派生类中都必须有 `draw()` 的实现，再比如 Animal 类中没有 `move()` 函数的实现，但是所有动物必须知道咋跑（假设他们都需要会跑），这时候就要用到抽象类。我们不能创建抽象类的对象。
> - 纯虚函数是我们只声明而没有实现的[虚函数](https://translate.googleusercontent.com/translate_c?depth=1&hl=zh-CN&pto=aue&rurl=translate.google.com&sl=auto&sp=nmt4&u=https://www.geeksforgeeks.org/virtual-functions-and-runtime-polymorphism-in-c-set-1-introduction/&usg=ALkJrhhJu8jDC6BB677fSQYeA4RJCAsyaQ)，我们通过在声明中分配0来声明纯虚函数。

```cpp
#include<iostream> 
using namespace std; 
  
class Base 
{ 
   int x; 
public: 
    virtual void fun() = 0; 
    int getX() { return x; } 
}; 
  
// This class inherits from Base and implements fun() 
class Derived: public Base 
{ 
    int y; 
public: 
    void fun() { cout << "fun() called"; } 
}; 
  
int main(void) 
{ 
    Derived d; 
    d.fun(); 
    return 0; 
} 
// fun() called
```

> - *如果一个类至少具有一个纯虚函数，则它是抽象的。* 
> - *我们可以有抽象类类型的指针和引用。*
> -  *如果我们不重写派生类中的纯虚函数，则派生类也将成为抽象类。*
> -  *抽象类可以具有构造函数。*

以上四点的🌰分别为：

````cpp
#include<iostream> 
using namespace std; 
  
class Test 
{ 
   int x; 
public: 
    virtual void show() = 0; 
    int getX() { return x; } 
}; 
  
int main(void) 
{ 
    Test t; 
    return 0; 
} 
// Compiler Error: cannot declare variable 't' to be of abstract
// type 'Test' because the following virtual functions are pure 
// within 'Test': note:     virtual void Test::show() 

#include<iostream> 
using namespace std; 
  
class Base 
{ 
public: 
    virtual void show() = 0; 
}; 
  
class Derived: public Base 
{ 
public: 
    void show() { cout << "In Derived \n"; } 
}; 
  
int main(void) 
{ 
    Base *bp = new Derived(); 
    bp->show(); 
    return 0; 
}
// In Derived 


#include<iostream> 
using namespace std; 
class Base 
{ 
public: 
    virtual void show() = 0; 
}; 
  
class Derived : public Base { }; 
  
int main(void) 
{ 
  Derived d; 
  return 0; 
}
// Compiler Error: cannot declare variable 'd' to be of abstract type 
// 'Derived'  because the following virtual functions are pure within
// 'Derived': virtual void Base::show() 


#include<iostream> 
using namespace std; 
  
// An abstract class with constructor 
class Base 
{ 
protected: 
   int x; 
public: 
  virtual void fun() = 0; 
  Base(int i) { x = i; } 
}; 
  
class Derived: public Base 
{ 
    int y; 
public: 
    Derived(int i, int j):Base(i) { y = j; } 
    void fun() { cout << "x = " << x << ", y = " << y; } 
}; 
  
int main(void) 
{ 
    Derived d(4, 5); 
    d.fun(); 
    return 0; 
} 
// x = 4, y = 5
````

- 好了，我们接着 Destructor 聊，`Virtual Destructor`：

  > 将基类析构函数设为虚函数可确保派生类的对象被正确地析构，即，基类和派生类的析构函数都将被调用。

```cpp
  #include<iostream> 
    
  using namespace std; 
    
  class base { 
    public: 
      base()      
      { cout<<"Constructing base \n"; } 
      virtual ~base() 
      { cout<<"Destructing base \n"; }      
  }; 
    
  class derived: public base { 
    public: 
      derived()      
      { cout<<"Constructing derived \n"; } 
      ~derived() 
      { cout<<"Destructing derived \n"; } 
  }; 
    
  int main(void) 
  { 
    derived *d = new derived();   
    base *b = d; 
    delete b; 
    getchar(); 
    return 0; 
  } 
// Constructing base
// Constructing derived
// Destructing derived
// Destructing base

// As a guideline, any time you have a virtual function in a class, you should immediately add a virtual destructor (even if it does nothing). This way, you ensure against any surprises later. 
```

纯虚析构函数感觉应该不会考，就不写了...感兴趣可以[戳我传送](https://www.geeksforgeeks.org/pure-virtual-destructor-c/)



##### 静态成员函数

在 C 语言中我们已经了解了静态变量，像变量一样，对象在声明为静态对象时也具有作用域，直到程序的生存期。比如：比较以下两个程序的不同之处。

```cpp
// CPP program to illustrate when not using static keyword 
#include<iostream> 
using namespace std; 
  
class GfG 
{ 
    int i; 
    public: 
        GfG() 
        { 
            i = 0; 
            cout << "Inside Constructor\n"; 
        } 
        ~GfG() 
        { 
            cout << "Inside Destructor\n"; 
        } 
}; 
  
int main() 
{ 
    int x = 0; 
    if (x==0) 
    { 
        GfG obj; 
    } 
    cout << "End of main\n"; 
} 
// Inside Constructor
// Inside Destructor
// End of main


// CPP program to illustrate class objects as static 
#include<iostream> 
using namespace std; 
  
class GfG 
{ 
    int i = 0; 
      
    public: 
    GfG() 
    { 
        i = 0; 
        cout << "Inside Constructor\n"; 
    } 
      
    ~GfG() 
    { 
        cout << "Inside Destructor\n"; 
    } 
}; 
  
int main() 
{ 
    int x = 0; 
    if (x==0) 
    { 
        static GfG obj; 
    } 
    cout << "End of main\n"; 
} 
// Inside Constructor
// End of main
// Inside Destructor
```

> **Static functions in a class**: Just like the static data  members or static variables inside the class, static member functions  also does not depend on object of class. We are allowed to invoke a  static member function using the object and the ‘.’ operator but it is  recommended to invoke the static members using the class name and the  scope resolution operator.
>  **Static member functions are allowed to access only the static data members or other static member functions**, they can not access the non-static data members or member functions of the class.

```cpp
// C++ program to demonstrate static member function in a class 
#include<iostream> 
using namespace std; 
  
class GfG 
{ 
   public: 
      
    // static member function 
    static void printMsg() 
    { 
        cout<<"Welcome to GfG!"; 
    } 
}; 
  
// main function 
int main() 
{ 
    // invoking a static member function 
    GfG::printMsg(); 
} 
// Welcome to GfG!
```

## Other Concepts

### 友元函数

> **Friend Class** A friend class can access private and protected members of other class in which it is declared as friend.  It is sometimes useful to allow a particular class to access private  members of other class.  For example a LinkedList class may be allowed  to access private members of Node.

```cpp

#include <iostream> 
class A { 
private: 
    int a; 
  
public: 
    A() { a = 0; } 
    friend class B; // Friend Class 
}; 
  
class B { 
private: 
    int b; 
  
public: 
    void showA(A& x) 
    { 
        // Since B is friend of A, it can access 
        // private members of A 
        std::cout << "A::a=" << x.a; 
    } 
}; 
  
int main() 
{ 
    A a; 
    B b; 
    b.showA(a); 
    return 0; 
} 
// A :: a = 0
```

> **Friend Function** Like friend class, a friend function  can be given special grant to access private and protected members. A  friend function can be:
>  a) A method of another class
>  b) A global function

```cpp
#include <iostream> 

class B; 

class A { 
public: 
	void showB(B&); 
}; 

class B { 
private: 
	int b; 

public: 
	B() { b = 0; } 
	friend void A::showB(B& x); // Friend function 
}; 

void A::showB(B& x) 
{ 
	// Since showB() is friend of B, it can 
	// access private members of B 
	std::cout << "B::b = " << x.b; 
} 

int main() 
{ 
	A a; 
	B x; 
	a.showB(x); 
	return 0; 
}
// B::b = 0
```

### 内联函数

> 如果一个函数是内联的，那么在编译时，编译器会把该函数的代码副本放置在每个调用该函数的地方。

```cpp
#include <iostream>
 
using namespace std;

inline int Max(int x, int y)
{
   return (x > y)? x : y;
}

int main( )
{

   cout << "Max (20,10): " << Max(20,10) << endl;
   cout << "Max (0,200): " << Max(0,200) << endl;
   cout << "Max (100,1010): " << Max(100,1010) << endl;
   return 0;
}
// Max (20,10): 20
// Max (0,200): 200
// Max (100,1010): 1010
```

### Template

> 感觉这里不会考，简单说就是把数据类型作为参数传入，我们就不用为不同的数据类型写一遍相似的代码了，和 TS 中的泛型概念一样。

举一个函数模版的🌰，比如用了模版的泡泡排序：

```cpp
#include <iostream> 
using namespace std; 
   
// A template function to implement bubble sort. 
// We can use this for any data type that supports 
// comparison operator < and swap works for it. 
template <class T> 
void bubbleSort(T a[], int n) { 
    for (int i = 0; i < n - 1; i++) 
        for (int j = n - 1; i < j; j--) 
            if (a[j] < a[j - 1]) 
              swap(a[j], a[j - 1]); 
} 
   
// Driver Code 
int main() { 
    int a[5] = {10, 50, 30, 40, 20}; 
    int n = sizeof(a) / sizeof(a[0]); 
   
    // calls template function  
    bubbleSort(a, 5); 
   
    cout << " Sorted array : "; 
    for (int i = 0; i < n; i++) 
        cout << a[i] << " "; 
    cout << endl; 
   
  return 0; 
} 
// Sorted array : 10 20 30 40 50
```

类模版和函数模版差不多，整个链表啊，二分树啊，栈啊之类的数据结构挺常用。比如这里实现了数组类：

```cpp
#include <iostream> 
using namespace std; 
  
template <typename T> 
class Array { 
private: 
    T *ptr; 
    int size; 
public: 
    Array(T arr[], int s); 
    void print(); 
}; 
  
template <typename T> 
Array<T>::Array(T arr[], int s) { 
    ptr = new T[s]; 
    size = s; 
    for(int i = 0; i < size; i++) 
        ptr[i] = arr[i]; 
} 
  
template <typename T> 
void Array<T>::print() { 
    for (int i = 0; i < size; i++) 
        cout<<" "<<*(ptr + i); 
    cout<<endl; 
} 
  
int main() { 
    int arr[5] = {1, 2, 3, 4, 5}; 
    Array<int> a(arr, 5); 
    a.print(); 
    return 0; 
} 
// 1 2 3 4 5
```

### Exception Handling

>  异常这个其实很符合英文语意，看看 `try...catch` 就 ok，这点和 JS 语法几乎完全一致。


