export {};

function addAge(constructor: Function) {
  constructor.prototype.age = 18;
}

@addAge
class Person {
  name: string;
  age!: number;
  constructor() {
    this.name = "ww";
  }
}

let person = new Person();
console.log(person.age); // 18

// 等同于 Person = addAge(function Person() { ... });

// 声明装饰器修饰方法/属性
function method(
  target: any,
  propertyKey: string,
  descriptor: PropertyDescriptor
) {
  console.log(target);
  console.log("prop " + propertyKey);
  console.log("desc " + JSON.stringify(descriptor) + "\n\n");
  descriptor.writable = false;
}

class Person1 {
  name: string;
  constructor() {
    this.name = "xiaomuzhu";
  }

  @method
  say() {
    return "instance method";
  }

  @method
  static run() {
    return "static method";
  }
}

const xmz = new Person1();

// 修改实例方法say
// xmz.say = function () {
//   return "edit";
// };

// 打印结果,检查是否成功修改实例方法
console.log(xmz.say());
