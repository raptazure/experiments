// 字面量类型 (Literal Type)
export {};

const a: 2333 = 2333; // ok
const ab: 0b10 = 2; // ok
const ao: 0o114 = 0b1001100; // ok
const ax: 0x514 = 0x514; // ok
const c: "xiaomuzhu" = "xiaomuzhu"; // ok
const d: false = false; // ok
// const g: 'gitlab' = 'github' // Type '"github"' is not assignable to type '"gitlab"'

// 字面量类型与联合类型结合时，可以模拟一个类似于枚举的效果
type Direction = "North" | "East" | "South" | "West";

function move(distance: number, direction: Direction) {
  // ...
}

move(1, "East");

// 类型字面量 (Type Literal)
type Foo = {
  baz: [number, "xiaozhu"];
  toString(): string;
  readonly [Symbol.iterator]: "github";
  0x1: "foo";
  bar: 12n;
};

// 可辨识联合类型
interface Info {
  username: string;
}

type UserAction =
  | {
      id: number;
      action: "delete";
      info: Info;
    }
  | {
      action: "create";
      info: Info;
    };

const UserReducer = (userAction: UserAction) => {
  switch (userAction.action) {
    case "delete":
      console.log(userAction.id);
      break;
    default:
      break;
  }
};

UserReducer({
  action: "delete",
  id: 111,
  info: {
    username: "meow",
  },
});
