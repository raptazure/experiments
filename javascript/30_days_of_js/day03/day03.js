/* Booleans */
// All numbers(positive and negative) are truthy except zero. All strings are truthy
let truValue = 4 > 3;
let isLightOn = true;

// 0, 0n, null, undefined, NaN, '', "", ``, empty string
let isHungry = false;

/* Undefined */
let firstName;
console.log(firstName);

/* Null */
let empty = null;
console.log(empty);

/* Operator */
let number = 5;
console.log(NaN == NaN);         // false, not equal
console.log(3 != '3');           // false, only compare value
console.log(3 !== '3');           // true, compare both value and data type
number > 0
  ? console.log(`${number} is a positive number`)
  : console.log(`${number} is a negative number`);

/* Window Methods */
alert('Welcome to 30DaysOfJavaScript')

let number0 = prompt('Enter number', 'number goes here');
console.log(number0);

const agree = confirm('Are you sure you like to delete? ');
console.log(agree);   // result will be true or false based on what you click on the dialog box

/* Date Object */
// Creating a time object
const now = new Date();
console.log(now);

console.log(now.getFullYear());
console.log(now.getMonth());    // month 0-11
console.log(now.getDate());     // day 1-31
console.log(now.getDay());      // weekday
console.log(now.getHours());    // hour 0-23
console.log(now.getMinutes());
console.log(now.getSeconds());
console.log(now.getTime());     // milliseconds since Jan 1, 1970

// Another way to get the unix time
const allSeconds = Date.now();
console.log(allSeconds);
console.log(allSeconds == new Date().getTime());   // true

// Format time
console.log(`${now.getDate()}/${now.getMonth() + 1}/${now.getFullYear()} ${now.getHours()}:${now.getMinutes()}`);
