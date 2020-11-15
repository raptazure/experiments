export {}

// 在函数名称后面声明泛型变量 <T>用于捕获开发者传入的参数类型（比如说string），然后我们就可以使用T(也就是string)做参数类型和返回值类型了
function returnItem<T>(para: T): T {
  return para
}

// 多个类型参数
function swap<T, U>(tuple: [T, U]): [U, T] {
  return [tuple[1], tuple[0]];
}

swap([7, 'seven']); // ['seven', 7]

// 泛型变量
function getArrayLength<T>(arg: Array<T>) {
  console.log((arg as Array<any>).length) // ok
  return arg
}

// 泛型接口
interface ReturnItemFn<T> {
  (para: T): T
}

const returnItem0: ReturnItemFn<number> = para => para

// 泛型类
class Stack<T> {
  private arr: T[] = []

  public push(item: T) {
      this.arr.push(item)
  }

  public pop() {
      this.arr.pop()
  }
}

type Params = number | string
class Stack1<T extends Params> {
  private arr: T[] = []

  public push(item: T) {
      this.arr.push(item)
  }

  public pop() {
      this.arr.pop()
  }
}

const stack1 = new Stack1<string>()
// const stack2 = new Stack1<boolean>()

// 泛型约束与索引类型
function getValue<T extends object, U extends keyof T>(obj: T, key: U) {
  return obj[key] // ok
}

const a = {
  name: 'xiaomuzhu',
  id: 1
}

getValue(a, 'id')

// 使用多重类型进行泛型约束
interface FirstInterface {
  doSomething(): number
}

interface SecondInterface {
  doSomethingElse(): string
}

interface ChildInterface extends FirstInterface, SecondInterface {

}

class Demo1<T extends ChildInterface> {
  private genericProperty: T

  useT() {
    this.genericProperty.doSomething()
    this.genericProperty.doSomethingElse()
  }
}

// 也可以利用交叉类型来进行多类型约束
class Demo2<T extends FirstInterface & SecondInterface> {
  private genericProperty: T

  useT() {
    this.genericProperty.doSomething() // ok
    this.genericProperty.doSomethingElse() // ok
  }
}

// 泛型与 new
function factory<T>(type: {new(): T}): T {
  return new type() // ok
}

// 设计泛型的关键目的是在成员之间提供有意义的约束，这些成员可以是：接口,类的实例成员,类的方法,函数参数,函数返回值