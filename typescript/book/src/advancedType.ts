export {}

// 交叉类型
interface IAnyObject {
  [prop: string]: any
}

function mixin<T extends IAnyObject, U extends IAnyObject>(first: T, second: U): T & U {
  const result = <T & U>{};
  for (let id in first) {
    (<T>result)[id] = first[id];
  }
  for (let id in second) {
    if (!result.hasOwnProperty(id)) {
      (<U>result)[id] = second[id];
    }
  }

  return result;
}

const x = mixin({ a: 'hello' }, { b: 42 });

// 现在 x 拥有了 a 属性与 b 属性
const a = x.a;
const b = x.b;

// 联合类型
function formatCommandline(command: string[] | string) {
  let line = '';
  if (typeof command === 'string') {
    line = command.trim();
  } else {
    line = command.join(' ').trim();
  }
}

// 类型别名
type some = boolean | string

const e: some = true // ok
const f: some = 'hello' // ok
// const g: some = 123 // 不能将类型“123”分配给类型“some”

// 类型别名可以是泛型, 也可以使用类型别名来在属性里引用自己
type Tree<T> = {
  value: T;
  left: Tree<T>;
  right: Tree<T>;
}

// interface 只能用于定义对象类型，而 type 的声明方式除了对象之外还可以定义交叉、联合、原始类型等，类型声明的方式适用范围显然更加广泛
// interface 方式可以实现接口的 extends 和 implements
// interface 可以实现接口合并声明
type Alias = { num: number }
interface Interface {
    num: number;
}
declare function aliased(arg: Alias): Alias;
declare function interfaced(arg: Interface): Interface;