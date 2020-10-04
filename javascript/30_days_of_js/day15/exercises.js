class Animal {
  constructor(name, age, color, legs) {
    this.name = name;
    this.age = age;
    this.color = color;
    this.legs = legs;
  }
  getInfo() {
    return `${this.name} is ${this.age}. He has ${this.legs} ${this.color} legs`;
  }
}

const cat = new Animal('meow', 10, 'black', 4);
const dog = new Animal('w', 8, 'green', 4);
console.log(cat.getInfo());
console.log(dog.getInfo());

class Animal0 extends Animal {
  constructor(name, age, color, legs, gender) {
    super(name, age, color, legs);
    this.gender = gender;
  }
  getInfo() {
    let pronoun = this.gender == 'Male' ? 'He' : 'She';
    return `${this.name} is ${this.age}. ${pronoun} has ${this.legs} ${this.color} legs`;
  }
}

const pig = new Animal0('h', 10, 'pink', 4, 'Female');
console.log(pig.getInfo());

class Statistics {
  constructor(arr) {
    this.arr = arr;
  }
  count() {
    return this.arr.length;
  }
  sum() {
    return this.arr.reduce((accu, curr) => accu + curr);
  }
  range() {
    this.arr.sort((a, b) => a - b);
    return this.arr[this.arr.length - 1] - this.arr[0];
  }
  median() {
    this.arr.sort((a, b) => a - b);
    return (this.arr[parseInt(this.arr.length / 2)] + this.arr[parseInt((this.arr.length - 1) / 2)]) / 2;
  }
  mode() {
    nums = {};
    this.arr.forEach(element => nums[element] = 0);
    this.arr.forEach(element => nums[element]++);
    return Object.keys(nums).reduce((a, b) => nums[a] > nums[b] ? a : b);
  }
  variance() {
    let nums = statistics.count(this.arr);
    let aver = statistics.sum(this.arr) / nums;
    return this.arr.reduce((accu, curr) => accu + (curr - aver) ** 2, 0) / nums;
  }
  std() {
    return Math.sqrt(statistics.variance(this.arr));
  }
}

const ages = new Statistics([31, 26, 34, 37, 27, 26, 32, 32, 26, 27, 27, 24, 32, 33, 27, 25, 26, 38, 37, 31, 34, 24, 33, 29, 26]);

console.log('Count:', ages.count());
console.log('Sum: ', ages.sum());
console.log('Range: ', ages.range());
console.log('Median: ', ages.median());
console.log('Mode: ', ages.mode());
console.log('Variance: ', ages.variance());
console.log('Standard Deviation: ', ages.std());

class PersonAccount {
  constructor(firstName, lastName, incomes) {
    this.firstName = firstName;
    this.lastName = lastName;
    this.incomes = incomes;
  }
  totalIncome() {
    const income = Object.keys(this.incomes);
    console.log(income);
    const res = parseInt(income[1]) + parseInt(income[0]) * 365;
    return res;
  }
  accountInfo() {
    return 'name: ' + this.firstName + ' ' + this.lastName;
  }
}

const person = new PersonAccount('ww', 'w', {
  233: 'daily',
  20000: 'yearly'
});

console.log(person.totalIncome());
console.log(person.accountInfo());