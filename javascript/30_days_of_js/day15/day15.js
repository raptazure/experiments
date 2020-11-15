// Class Constructor
class Person {
  constructor(firstName, lastName) {
    console.log(this);
    this.firstName = firstName;
    this.lastName = lastName;
  }
}

const person = new Person();
const person1 = new Person('meow', 'ww');
console.log(person);
console.log(person1);

// Default values with constructor
class Person1 {
  constructor(firstName = 'Asabeneh', lastName = 'Yetayeh', age = 250, country = 'Finland', city = 'Helsinki') {
    this.firstName = firstName;
    this.lastName = lastName;
    this.age = age;
    this.country = country;
    this.city = city;
  }
}

const person2 = new Person1(); // it will take the default values
const person3 = new Person1('Lidiya', 'Tekle', 28, 'Finland', 'Espoo');

console.log(person1);
console.log(person2);

// Class methods
class Person2 {
  constructor(firstName, lastName, age, country, city) {
    this.firstName = firstName;
    this.lastName = lastName;
    this.age = age;
    this.country = country;
    this.city = city;
    this.score = 0;
    this.skills = [];
  }
  getFullName() {
    const fullName = this.firstName + ' ' + this.lastName;
    return fullName;
  }
}

const person4 = new Person2('Lidiya', 'Tekle', 28, 'Finland', 'Espoo');
console.log(person4.score);
console.log(person4.getFullName());

// getter and setter
class Person3 {
  constructor(firstName, lastName, age, country, city) {
    this.firstName = firstName;
    this.lastName = lastName;
    this.age = age;
    this.country = country;
    this.city = city;
    this.score = 0;
    this.skills = [];
  }
  getFullName() {
    const fullName = this.firstName + ' ' + this.lastName;
    return fullName;
  }
  get getScore() {
    return this.score;
  }
  get getSkills() {
    return this.skills;
  }
  set setScore(score) {
    this.score += score;
  }
  set setSkill(skill) {
    this.skills.push(skill);
  }
  getPersonInfo() {
    let fullName = this.getFullName();
    let skills =
      this.skills.length > 0 &&
      this.skills.slice(0, this.skills.length - 1).join(', ') +
      ` and ${this.skills[this.skills.length - 1]}`;
    let formattedSkills = skills ? `He knows ${skills}` : '';

    let info = `${fullName} is ${this.age}. He lives ${this.city}, ${this.country}. ${formattedSkills}`;
    return info;
  }
}

const person5 = new Person3('Asabeneh', 'Yetayeh', 250, 'Finland', 'Helsinki');
const person6 = new Person3('Lidiya', 'Tekle', 28, 'Finland', 'Espoo');

person5.setScore = 1;
person5.setSkill = 'HTML';
person5.setSkill = 'CSS';
person5.setSkill = 'JavaScript';

console.log(person5.getSkills);
console.log(person6.getSkills);

person6.setScore = 1;
person6.setSkill = 'Planning';
person6.setSkill = 'Managing';
person6.setSkill = 'Organizing';

console.log(person5.score);
console.log(person6.score);

console.log(person5.skills);
console.log(person6.skills);
console.log(person6.getPersonInfo());

// Static method
// The static keyword defines a static method for a class. Static methods are not called on instances of the class. Instead, they are called on the class itself. These are often utility functions, such as functions to create or clone objects. An example of static method is Date.now().
class Person4 {
  constructor(firstName, lastName, age, country, city) {
    this.firstName = firstName;
    this.lastName = lastName;
    this.age = age;
    this.country = country;
    this.city = city;
    this.score = 0;
    this.skills = [];
  }
  getFullName() {
    const fullName = this.firstName + ' ' + this.lastName;
    return fullName;
  }
  get getScore() {
    return this.score;
  }
  get getSkills() {
    return this.skills;
  }
  set setScore(score) {
    this.score += score;
  }
  set setSkill(skill) {
    this.skills.push(skill);
  }
  getPersonInfo() {
    let fullName = this.getFullName();
    let skills =
      this.skills.length > 0 &&
      this.skills.slice(0, this.skills.length - 1).join(', ') +
      ` and ${this.skills[this.skills.length - 1]}`;

    let formattedSkills = skills ? `He knows ${skills}` : '';

    let info = `${fullName} is ${this.age}. He lives ${this.city}, ${this.country}. ${formattedSkills}`;
    return info;
  }
  static favoriteSkill() {
    const skills = ['HTML', 'CSS', 'JS', 'React', 'Python', 'Node'];
    const index = Math.floor(Math.random() * skills.length);
    return skills[index];
  }
  static showDateTime() {
    let now = new Date();
    let year = now.getFullYear();
    let month = now.getMonth() + 1;
    let date = now.getDate();
    let hours = now.getHours();
    let minutes = now.getMinutes();
    if (hours < 10) {
      hours = '0' + hours;
    }
    if (minutes < 10) {
      minutes = '0' + minutes;
    }

    let dateMonthYear = date + '.' + month + '.' + year;
    let time = hours + ':' + minutes;
    let fullTime = dateMonthYear + ' ' + time;
    return fullTime;
  }
}

console.log(Person4.favoriteSkill());
console.log(Person4.showDateTime());

// Inheritance
// Using inheritance we can access all the properties and the methods of the parent class. This reduces repetition of code.
class Student extends Person3 {
  saySomething() {
    console.log('I am a child of the person class');
  }
}

const s = new Student('Asabeneh', 'Yetayeh', 'Finland', 250, 'Helsinki');
console.log(s);
console.log(s.getPersonInfo());
s.saySomething();

// Overriding methods
// As you can see, we manage to access all the methods in the Person Class and we used it in the Student child class. We can customize the parent methods, we can add additional properties to a child class. If we want to customize, the methods and if we want to add extra properties, we need to use the constructor function the child class too. Inside the constructor function we call the super() function to access all the properties from the parent class. The Person class didn't have gender but now let us give gender property for the child class, Student. If the same method name used in the child class, the parent method will be overridden.
class Student1 extends Person4 {
  constructor(firstName, lastName, age, country, city, gender) {
    super(firstName, lastName, age, country, city);
    this.gender = gender;
  }

  saySomething() {
    console.log('I am a child of the person class');
  }
  getPersonInfo() {
    let fullName = this.getFullName();
    let skills =
      this.skills.length > 0 &&
      this.skills.slice(0, this.skills.length - 1).join(', ') +
      ` and ${this.skills[this.skills.length - 1]}`;

    let formattedSkills = skills ? `He knows ${skills}` : '';
    let pronoun = this.gender == 'Male' ? 'He' : 'She';

    let info = `${fullName} is ${this.age}. ${pronoun} lives in ${this.city}, ${this.country}. ${formattedSkills}`;
    return info;
  }
}

const s1 = new Student1('Asabeneh', 'Yetayeh', 250, 'Finland', 'Helsinki', 'Male');
const s2 = new Student1('Lidiya', 'Tekle', 28, 'Finland', 'Helsinki', 'Female');
s1.setScore = 1;
s1.setSkill = 'HTML';
s1.setSkill = 'CSS';
s1.setSkill = 'JavaScript';

s2.setScore = 1;
s2.setSkill = 'Planning';
s2.setSkill = 'Managing';
s2.setSkill = 'Organizing';

console.log(s1);

s1.saySomething();
console.log(s1.getFullName());
console.log(s1.getPersonInfo());

s2.saySomething();
console.log(s2.getFullName());
console.log(s2.getPersonInfo());