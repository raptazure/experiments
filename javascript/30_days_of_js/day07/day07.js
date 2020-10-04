/* Function Declaration */
function printFullName(firstName, lastName) {
  return `${firstName} ${lastName}`
}
console.log(printFullName('meow', 'cat'))

// Function with default parameters
function weightOfObject(mass, gravity = 9.81) {
  let weight = mass * gravity + ' N';
  return weight;
}
console.log('Weight of an object in Newton: ', weightOfObject(100));
console.log('Weight of an object in Newton: ', weightOfObject(100, 1.62));

// Unlimited number of parameters
function sumAllNums() {
  let sum = 0;
  for (let i = 0; i < arguments.length; i++) {
    sum += arguments[i];
  }
  return sum;
}

console.log(sumAllNums(1, 2, 3, 4));          // 10
console.log(sumAllNums(10, 20, 13, 40, 10));  // 93


/* Arrow Function */
const areaOfCircle = (radius) => {
  let area = Math.PI * radius * radius;
  return area;
};

console.log(areaOfCircle(10));

const square0 = n => n * n;
console.log(square0(2));

const changeToUpperCase = arr => {
  const newArr = [];
  for (const element of arr) {
    newArr.push(element.toUpperCase());
  }
  return newArr;
};

const countries = ['Finland', 'Sweden', 'Norway', 'Denmark', 'Iceland'];
console.log(changeToUpperCase(countries));

const printFullNameArrow = (firstName, lastName) => `${firstName} ${lastName}`;
console.log(printFullNameArrow('meow', 'kitty'));

// Function with default parameters
const calAge = (birthYear, currentYear = 2020) => currentYear - birthYear;
console.log('Age:', calAge(2001));

// Unlimited number of parameters
const sumNums = (...args) => {
  let sum = 0;
  for (const element of args) {
    sum += element;
  }
  return sum;
};

console.log(sumNums(1, 2, 3, 4, 5));

/* Anonymous Function */
const anonymousFun = function() {
  console.log('I am an anonymous function and my value is stored in anonymousFun');
};

// Function expressions
const square = function(n) {
  return n * n;
};
console.log(square(2));   // -> 4

// Self invoking function
let squareNum = (function(n) {
  return n * n;
})(10);
console.log(squareNum);
