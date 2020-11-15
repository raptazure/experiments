export {}
// 抽象类
abstract class Animal {
  abstract makeSound(): void;
  move(): void {
      console.log('roaming the earch...');
  }
}

class Cat extends Animal {

  makeSound() {
      console.log('miao miao')
  }
}

const cat = new Cat()

cat.makeSound() // miao miao
cat.move() // roaming the earch...

// protected
class Car {
  protected run() {
      console.log('启动...')
  }
}

class GTR extends Car {
  init() {
      this.run()
  }
}

const car = new Car()
const gtr = new GTR()

gtr.init() // 启动...
// car.run() // [ts] 属性“run”受保护，只能在类“Car”及其子类中访问。
// gtr.run() // [ts] 属性“run”受保护，只能在类“Car”及其子类中访问。

// class 可以作为接口
/**
 * export default class Props {
 *    public children: Array<React.ReactElement<any>> | React.ReactElement<any> | never[] = []
 *    public speed: number = 500
 *    public height: number = 160
 *    public animation: string = 'easeInOutQuad'
 *    public isAuto: boolean = true
 *    public autoPlayInterval: number = 4500
 *    public afterChange: () => {}
 *    public beforeChange: () => {}
 *    public selesctedColor: string
 *    public showDots: boolean = true
 *  }
 * 
 * 当我们需要传入 props 类型的时候直接将 Props 作为接口传入，此时 Props 的作用就是接口，而当需要我们设置defaultProps初始值的时候，我们只需要:
 * 
 * public static defaultProps = new Props()
 */
