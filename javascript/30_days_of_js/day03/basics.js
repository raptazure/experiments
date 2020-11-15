// String
const s = "hello, world";
console.log(s.split(','));

// Array
const numbers = new Array(1, 2, 3, 4, 5);
console.log(numbers);
fruits = ['apple', 'banana', 'oranges', true, 10];
fruits.push('mangos');
console.log(fruits)
fruits.unshift('strawberries');
console.log(fruits);
fruits.pop();
console.log(Array.isArray('hello'));
console.log(fruits.indexOf('oranges'));

// Object literals
const person = {
    firstName: 'John',
    lastName: 'Doe',
    age: 30,
    hobbies: ['music', 'movies', 'sports'],
    address: {
        street: '20 main st',
        city: 'Boston'
    }
}
console.log(person.address.street);

const {
    firstName,
    lastName,
    address: {
        city
    }
} = person;
console.log(city);

person.email = 'xxx@gamil.com';
console.log(person);

const todos = [{
        id: 1,
        txt: 'wake up',
        isCompleted: true
    },
    {
        id: 2,
        txt: 'learn js',
        isCompleted: true
    },
    {
        id: 3,
        txt: 'learn dom',
        isCompleted: false
    }
];

console.log(todos[1].txt);

const todoJSON = JSON.stringify(todos);
console.log(todoJSON);

// For
for (let i = 0; i < todos.length; i++) {
    console.log(`todo: ${todos[i].txt}`);
}

// forEach
todos.forEach(function (todo) {
    console.log(todo.txt);
});

// map
const todoText = todos.map(function (todo) {
    return todo.txt;
});

console.log(todoText);

// filter
const todoCompleted = todos.filter(function (todo) {
    return todo.isCompleted === true;
}).map(function (todo) {
    return todo.txt;
})

console.log(todoCompleted);

// if
const x = 10;
if (x === 10) {
    console.log('x is 10');
} else {
    console.log('x is not 10');
}

const color = x > 10 ? 'red' : 'blue';
console.log(color);

switch (color) {
    case "red":
        console.log('color#1');
        break;
    case "blue":
        console.log('color#2');
        break;
    default:
        console.log('Unkonwn');
}

//functions
const addNums = (num1 = 1, num2 = 1) => {
    return num1 + num2;
}

console.log(addNums(5));

const addNums1 = (num1, num2) => num1 + num2;
console.log(addNums1(5, 5));

const multiNums = num1 => num1 * 5;
console.log(multiNums(9));

// Constructor function
function Person(firstName, lastName, dob) {
    this.firstName = firstName;
    this.lastName = lastName;
    this.dob = new Date(dob);
    this.getFullName = function () {
        return `${this.firstName} ${this.lastName}`;
    }
}

Person.prototype.getBirthYear = function () {
    return this.dob.getFullYear();
}

// Class 
class PersonCl {
    constructor(firstName, lastName, dob) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.dob = new Date(dob);
    }

    getBirthYear() {
        return this.dob.getFullYear();
    }

    getFullName() {
        return `${this.firstName} ${this.lastName}`;
    }
}

// Instantiate object
const person1 = new Person('rapt', 'ozone', '2-2-2222');
console.log(person1.dob);

console.log(person1.getBirthYear());
console.log(person1.getFullName());

const person2 = new PersonCl('du','lu','1-1-1111');
console.log(person2.getBirthYear());