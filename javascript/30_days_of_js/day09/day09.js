// Callback
const callback = (n) => {
  return n ** 2
}

function cube(callback, n) {
  return callback(n) * n
}

console.log(cube(callback, 3)) // 27

// Returning function
const higherOrder = n => {
  const doSomething = m => {
    const doWhatEver = t => {
      return 2 * n + 3 * m + t
    }
    return doWhatEver
  }
  return doSomething
}
console.log(higherOrder(2)(3)(10)) // 23

const numbers = [1, 2, 3, 4]
const sumArray = arr => {
  let sum = 0
  const callBack = function (element) {
    sum += element
  }
  numbers.forEach(callBack)
  return sum
}
console.log(sumArray(numbers)) // 10

// Setting time
function sayHello() {
  console.log("hello")
}
setInterval(sayHello, 2000) // it prints hello in every 2 seconds

function sayHello1() {
  console.log("Hello")
}
setTimeout(sayHello1, 200) // it prints hello after it waits for 2 seconds.

// forEach - Iterate an array elements. We use forEach only with arrays. It takes a callback function with elements, index parameter and array itself. The index and the array optional.
let sum = 0
numbers.forEach(num => sum += num)
console.log(sum) // 10
numbers.forEach(num => console.log(num))

const countries = ['Finland', 'Denmark', 'Sweden', 'Norway', 'Iceland']
countries.forEach(country => console.log(country.toUpperCase()))

// map - Iterate an array elements and modify the array elements. It takes a callback function with elements and index parameter and return a new array.
const numberSquare = numbers.map(num => num ** 2)
console.log(numberSquare)

// 1. explicit return arrow function
const countryUpperCase = countries.map(country => country.toUpperCase())
console.log(countryUpperCase)

// 2. arrow function
const countriesToUpperCase = countries.map((country) => {
  return country.toUpperCase();
})

const countriesFirstThreeLetters = countries.map(country => country.toUpperCase().slice(0, 3))
console.log(countriesFirstThreeLetters)

// filter - Filter out items which full fill filtering conditions and return a new array.
const countriesContainingLand = countries.filter(country => country.includes('land'))
console.log(countriesContainingLand)

const countriesHaveFiveLetters = countries.filter(country => country.length === 6)
console.log(countriesHaveFiveLetters)

const scores = [{
    name: 'Asabeneh',
    score: 95
  },
  {
    name: 'Mathias',
    score: 80
  },
  {
    name: 'Elias',
    score: 50
  },
  {
    name: 'Martha',
    score: 85
  },
  {
    name: 'John',
    score: 100
  }
]

const scoresGreaterEight = scores.filter(score => score.score > 80)
console.log(scoresGreaterEight)

// reduce - Reduce takes a callback function. The call back function takes accumulator and current value as a parameter and returns a single value
const sum0 = numbers.reduce((accum, curr) => accum + curr)
console.log(sum0)

// every - Check if all the elements are similar in one aspect. It returns boolean
const strs = ['233', 'come', 'on']
const areAllStr = strs.every(name => typeof name === 'string')
console.log(areAllStr) // true

const bools = [true, true]
const areAllTrue = bools.every(b => {
  return b === true
})
console.log(areAllTrue) // true

// find - Return the first element which satisfies the condition
const ages = [24, 22, 25, 32, 35, 18]
const age = ages.find(age => age < 20)
console.log(age)

const score0 = scores.find(user => {
  return user.score > 80
})
console.log(score0)

// findIndex - Return the position of the first element which satisfies the condition
const age0 = ages.findIndex(age => age < 20)
console.log(age0) // 5

// some - Check if some of the elements are similar in one aspect. It returns boolean
const areAllNum = strs.some(str => typeof str === 'number')
console.log(areAllNum) // false

// sort - By default, the sort() method sorts values as strings.
// Sorting string values
const products = ['Milk', 'Coffee', 'Sugar', 'Honey', 'Apple', 'Carrot']
console.log(products.sort()) // Now the original products array is also sorted

// Sorting numeric values
const toSortNums = [9.81, 3.14, 37, 100]
numbers.sort(function (a, b) {
  return a - b
})

console.log(numbers) // [3.14, 9.81, 37, 100]

numbers.sort(function (a, b) {
  return b - a
})
console.log(numbers) // [100, 37, 9.81, 3.14]

// Sorting object arrays
const users = [{
    name: 'Asabeneh',
    age: 150
  },
  {
    name: 'Brook',
    age: 50
  },
  {
    name: 'Eyo',
    age: 100
  },
  {
    name: 'Elias',
    age: 22
  }
]
users.sort((a, b) => {
  return a.age - b.age
})
console.log(users) // sorted ascending

users.sort((a, b) => { // or a.age
  if (a['age'] < b['age']) return 1
  if (a['age'] > b['age']) return -1
  return 0
})
console.log(users)