enum Direction0 {
  Up,
  Down,
  Left,
  Right
}

console.log(Direction0.Up === 0); // true
console.log(Direction0.Down === 1); // true
console.log(Direction0.Left === 2); // true
console.log(Direction0.Right === 3); // true

// 反向映射
/*
var Direction;
(function (Direction) {
  Direction[Direction["Up"] = 10] = "Up";
  Direction[Direction["Down"] = 11] = "Down";
  Direction[Direction["Left"] = 12] = "Left";
  Direction[Direction["Right"] = 13] = "Right";
})(Direction || (Direction = {}));
console.log(Direction0[0]); // Up
*/
enum Direction1 {
  Up = 10,
  Down,
  Left,
  Right
}

console.log(Direction1.Up, Direction1.Down, Direction1.Left, Direction1.Right); // 10 11 12 13
console.log(Direction1[10], Direction1['Right']); // Up 13

enum Direction2 {
  Up = 'Up',
  Down = 'Down',
  Left = 'Left',
  Right = 'Right'
}

console.log(Direction2['Right'], Direction2.Up); // Right Up

enum BooleanLikeHeterogeneousEnum {
  No = 0,
  Yes = "YES",
}

const enum Direction3 {
  Up = 'Up',
  Down = 'Down',
  Left = 'Left',
  Right = 'Right'
}

// var a0 = "Up";
const a0 = Direction3.Up;

// 当所有枚举成员都拥有字面量枚举值时，它就带有了一种特殊的语义，即枚举成员成为了类型
enum Direction4 {
  Up,
  Down,
  Left,
  Right
}

const a1 = 0;
console.log(a1 === Direction4.Up); // true

type c = 0

declare let b: c

// b = 1 // 不能将类型“1”分配给类型“0”
b = Direction4.Up // ok

// 联合枚举类型
enum Direction5 {
  Up,
  Down,
  Left,
  Right
}

declare let a2: Direction5

enum Animal {
  Dog,
  Cat
}

// 联合类型 Direction.Up | Direction.Down | Direction.Left | Direction.Righ
a2 = Direction5.Up // ok
// a2 = Animal.Dog // 不能将类型“Animal.Dog”分配给类型“Direction”

// 枚举合并
enum Direction6 {
  Up = 'Up',
  Down = 'Down',
  Left = 'Left',
  Right = 'Right'
}

enum Direction6 {
  Center = 1
}

// 借助 namespace 可以给枚举添加静态方法
enum Month {
  January,
  February,
  March,
  April,
  May,
  June,
  July,
  August,
  September,
  October,
  November,
  December,
}

function isSummer(month: Month) {
  switch (month) {
      case Month.June:
      case Month.July:
      case Month.August:
          return true;
      default:
          return false
  }
}

namespace Month {
  export function isSummer(month: Month) {
      switch (month) {
          case Month.June:
          case Month.July:
          case Month.August:
              return true;
          default:
              return false
      }
  }
}

console.log(Month.isSummer(Month.January)) // false