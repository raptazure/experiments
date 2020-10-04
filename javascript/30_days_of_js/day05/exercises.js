itCompanies = ['Facebook', 'Google', 'Microsoft', 'Apple', 'IBM', 'Oracle', 'Amazon'];
console.log("the middle one is:", itCompanies[itCompanies.length >> 1]);
for (let i = 0; i < itCompanies.length; i++) {
  itCompanies[i] = itCompanies[i].toUpperCase();
  console.log(itCompanies[i]);
}

function titleCase(s) {
  s = s.toLowerCase();
  return s.substr(0, 1).toUpperCase() + s.substr(1).toLowerCase();
}

function moreThanOne(arr) {
  let ans = [];
  for (let i = 0; i < arr.length; i++) {
    arr[i] = titleCase(arr[i]);
    if (arr[i].indexOf('o') != arr[i].lastIndexOf('o')) {
      ans.push(arr[i]);
    }
  }
  return ans;
}
console.log(moreThanOne(itCompanies));

console.log(itCompanies.sort());
console.log(itCompanies.reverse());
console.log(itCompanies.slice(0, 3));
console.log(itCompanies.slice(itCompanies.length >> 1, (itCompanies.length + 1) >> 1));
itCompanies.splice(0, itCompanies.length);
console.log(itCompanies);

countries = require('./countries.js');
console.log(`The middle country is ${countries[countries.length >> 1]}`);
if (!countries.includes('Ethiopia')) {
  countries.push('Ethiopia');
} else {
  console.log("ETHIOPIA");
}

let text =
  'I love teaching and empowering people. I teach HTML, CSS, JS, React, Python.'
let textAfter = text.replace(/[^a-zA-Z\d\s]/g, '');
let words = textAfter.split(' ');
console.log(`the number of words is ${words.length}`);

const shoppingCart = ['Milk', 'Coffee', 'Tea', 'Honey'];
if (!shoppingCart.includes('Meat') && !shoppingCart.includes('Surgar')) {
  shoppingCart.unshift('Meat');
  shoppingCart.push('Sugar');
}
shoppingCart[shoppingCart.indexOf('Tea')] = 'Green Tea';
console.log(shoppingCart);

const ages = [19, 22, 19, 24, 20, 25, 26, 24, 25, 24];
ages.sort();
console.log(`min: ${ages[0]}, max: ${ages[ages.length - 1]}`);
console.log(`median age: ${ages[ages.length >> 1] + ages[ages.length >> 2 + 1] >> 1}`);
const sum = ages.reduce((a, b) => a + b, 0);
console.log(`average age: ${sum / ages.length}`);


