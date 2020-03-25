export {}

// 结构类型: 一种只使用其成员来描述类型的方式，其基本规则是，如果 x 要兼容 y，那么 y 至少具有与 x 相同的属性
class Person {
  constructor(public weight: number, public name: string, public born: string) {

  }
}

interface Dog {
  name: string
  weight: number
}

let x: Dog
x = new Person(120, 'www', '2000-03-15')

// 函数的类型兼容性
let x1 = (a: number) => 0;
let y = (b: number, s: string) => 0;

y = x1; // OK
// x1 = y; // Error 不能将类型“(b: number, s: string) => number”分配给类型“(a: number) => number”。

let foo = (x: number, y: number) => {};
let bar = (x?: number, y?: number) => {};
let bas = (...args: number[]) => {};

foo = bar = bas;
bas = bar = foo;

// 枚举的类型兼容性
enum Status {
  Ready,
  Waiting
}

let status = Status.Ready;
let num = 0;

status = num;
num = status;

// 类的类型兼容性
class Animal {
  feet: number;
  constructor(name: string, numFeet: number) {}
}

class Size {
  feet: number;
  constructor(meters: number) {}
}

let a: Animal;
let s: Size;

a = s; // OK
s = a; // OK

// 私有的和受保护的成员必须来自于相同的类
class Animal1 {
  protected feet: number;
}
class Cat extends Animal1 {}

let animal: Animal1;
let cat: Cat;

animal = cat; // ok
cat = animal; // ok

class Size1 {
  protected feet: number;
}

let size: Size1;

// animal = size; // ERROR
// size = animal; // ERROR

// 泛型的类型兼容性
interface Person1<T> {

}

let x2 : Person1<string>
let y2 : Person1<number>

x2 = y2 // ok
y2 = x2 // ok

interface Person2<T> {
  name: T
}

let x3 : Person2<string>
let y3 : Person2<number>

// x3 = y3 // 不能将类型“Person2<number>”分配给类型“Person2<string>”。
// y3 = x3 // 不能将类型“Person2<string>”分配给类型“Person2<number>”。