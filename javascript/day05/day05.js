/* Create an array */
const arr = Array(8);
const arr0 = [];
console.log(arr);

const fruits = ['banana', 'orange', 'mango', 'lemon'];
console.log('Number of fruits:', fruits.length);

// it can contain different data types
const arr1 = [
    'Asabeneh',
    250,
    true,
    {
        country: 'Finland',
        city: 'Helsinki'
    },
    {
        skills: ['HTML', 'CSS', 'JS', 'React', 'Python']
    }
];
console.log(arr1);

// using split
let companiesString = 'Facebook, Google, Microsoft, Apple, IBM, Oracle, Amazon';
const companies = companiesString.split(',');

// creating static values with fill
const eightXvalues = Array(8).fill('X');

/* Concatenating array */
const vegetables = ['Tomato', 'Potato', 'Carrot'];
const fruitsAndVegetables = fruits.concat(vegetables);
console.log(fruitsAndVegetables);

/* Getting index */
const numbers = [1, 2, 3, 4, 5, 3, 1, 2]
console.log(numbers.indexOf(5));      // 4
console.log(numbers.lastIndexOf(2));  // 7

/* Check an element */
let index = fruits.indexOf('banana'); // 0
index != -1 
  ? console.log('This fruit does exist in the array') 
  : console.log('This fruit does not exist in the array');

console.log(fruits.includes('banana'));

/* Checking array */
console.log(Array.isArray(numbers));

/* Checking array */
console.log(numbers.toString());

/* Joining array elements */
const names = ['Asabeneh', 'Mathias', 'Elias', 'Brook'];
console.log(names.join(' # '));

/* Slicing array elements */
// it doesn't include the ending position
console.log(numbers.slice(1, 4));

/* Splice method in array */
console.log(numbers.splice(3, 3, 6, 7, 8));  // [ 4, 5, 3 ]
console.log(numbers);                        // [ 1, 2, 3, 6, 7, 8, 1, 2 ]
console.log(numbers.splice(0, 1));           // remove the first item

/* Adding item to an array using push */
fruits.push('apple')
console.log(fruits)

/* Removing the end element using pop */
fruits.pop();
console.log(fruits);

/* Add an element from the beginning */
fruits.unshift('apple');
console.log(fruits);

/* Reversing array order */
fruits.reverse();
console.log(fruits);

/* Sorting elements in array */
const webTechs = [
    'HTML',
    'CSS',
    'JavaScript',
    'React',
    'Redux',
    'Node',
    'MongoDB'
  ];
webTechs.sort();
console.log(webTechs);

/* Array of arrays */
const frontEnd = ['HTML', 'CSS', 'JS', 'React', 'Redux'];
const backEnd = ['Node','Express', 'MongoDB'];
const fullStack = [frontEnd, backEnd];
console.log(fullStack[1]);