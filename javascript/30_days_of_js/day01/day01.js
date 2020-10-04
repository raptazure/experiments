let firstName;
console.log(firstName); // not defined, because it is not assigned to a value yet
let emptyValue = null   // null in JavaScript means an empty value.
console.log(typeof 'Asabeneh') // string
console.log(typeof 5)          // number
console.log(typeof true )      // boolean
console.log(typeof null)       // object type
console.log(typeof undefined)  // undefined

// Declaring different variables of different data types
firstName = 'Asabeneh'  // first name of a person
let lastName = 'Yetayeh'    // last name of a person
let country = 'Finland'     // country
let city = 'Helsinki'       // capital city
let age = 100               // age in years
let isMarried = true

console.log(firstName, lastName, country, city, age, isMarried); //Asabeneh, Yetayeh, Finland, Helsinki, 100, True

// Declaring variables with number values
const gravity = 9.81      // earth gravity  in m/s2
const boilingPoint = 100  // water boiling point, temperature in oC
const PI = 3.14           // geometrical constant

console.log(gravity, boilingPoint, PI); // 9.81, 100, 3.14
// Variables can also be declaring in one line separated by comma
let name = 'Asabeneh',
  job = 'teacher',
  live = 'Finland';
console.log('my name is ' + name, "and I am a " + job, "in " + live);