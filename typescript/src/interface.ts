interface Speak {
  (words: string) : string
}

interface User {
  name: string
  age?: number
  readonly isMale: boolean
  say: (words: string) => string
  speak: Speak
}

const getUserName = (user: User) => user.name

interface Config {
  width?: number;
}

function  CalculateAreas(config: Config): { area: number} {
  let square = 100;
  if (config.width) {
      square = config.width * config.width;
  }
  return {area: square};
}

// 对象字面量当被赋值给变量或作为参数传递的时候，会被特殊对待而且经过“额外属性检查”。 如果一个对象字面量存在任何“目标类型”不包含的属性时，会得到一个错误
// error: 'widdth' not expected in type 'Config'
// let mySquare = CalculateAreas({ widdth: 5 });

/** 如何解决？
 * 
 * 类型断言
 * let mySquare = CalculateAreas({ widdth: 5 } as Config);
 * 
 * 添加字符串索引签名
 * interface Config {
 *   width?: number;
 *   [propName: string]: any;
 * }
 * 
 * 将字面量赋值给另外一个变量
 * let options: any = { widdth: 5 };
 * let mySquare = CalculateAreas(options);
 */

interface Phone {
  [name: string]: string
}

interface User0 {
  name: string
  age?: number
  readonly isMale: boolean
  say: () => string
  phone: Phone
}

// 继承接口
interface VIPUser extends User {
  broadcast: () => void
}

// interface newbee extends User, SuperUser {
//   broadcast: () => void
// }