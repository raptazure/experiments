let notSure: any = 4;
notSure = "maybe a string instead";

let value0: any;

value0 = true;             // OK
value0 = 1;                // OK
value0 = "Hello World";    // OK
value0 = Symbol("type");   // OK
value0 = {};               // OK
value0 = [];               // OK

let value1: unknown;

value1 = true;             // OK
value1 = 1;                // OK
value1 = "Hello World";    // OK
value1 = Symbol("type");   // OK
value1 = {};               // OK
value1 = [];               // OK

let value2: any;

value2.foo.bar;  // OK
value2();        // OK
new value2();    // OK
value2[0][1];    // OK

let value: unknown;

// value.foo.bar;  // ERROR
// value();        // ERROR
// new value();    // ERROR
// value[0][1];    // ERROR

function getValue(value: unknown): string {
  if (value instanceof Date) { 
    // 因已把value的类型缩小为Date实例的范围内,所以value.toISOString()
    return value.toISOString();
  }

  return String(value);
}

// 抛出异常的函数永远不会有返回值
function error(message: string): never {
  throw new Error(message);
}

// 空数组，而且永远是空的
const empty: never[] = [];

const list0: Array<number> = [1, 2, 3];
const list1: number[] = [1, 2, 3];

let x: [string, number];
// x = ['hello', 10, false] // Error
// x = ['hello'] // Error
// x = [10, 'hello']; // Error
x = ['hello', 10]; // OK

// [string, number]
interface Tuple extends Array<string | number> {
  0: string;
  1: number;
  length: 2;
}

const tuple: [string, number] = ['a', 1];
tuple.push(2); // ok
console.log(tuple); // ["a", 1, 2] -> 正常打印出来

// console.log(tuple[2]); // Tuple type '[string, number]' of length '2' has no element at index '2'

enum Direction {
  Center = 1
}

let valueObj: object;

// 普通对象、枚举、数组、元组都是 object 类型
valueObj = Direction;
valueObj = [1];
value = [1, 'hello']
value = {}