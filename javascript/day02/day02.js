/*
Primitive data types in JavaScript includes:
Numbers - Integers, floats
Strings - Any data under single or double quote
Booleans - true or false value
Null - empty value or no value
Undefined - a declared variable without a value

Non-primitive data types in JavaScript includes:
Objects
Functions
Arrays

Once a primitive data type is created we can not modify it.
Non-primitive data types are modifiable or mutable.
Do not compare non-primitive data types. Do not compare array, function, or object.
*/
let userOne = {
    name: 'rapt',
    age: 18
}
let userTwo = {
    name: 'azure',
    age: 18
}
console.log(userOne == userTwo)  // false, compared by referebce instaed of values

// math object
const PI = Math.PI
console.log(Math.round(PI))
console.log(Math.floor(PI))
console.log(Math.ceil(PI))

// random() method generates number from 0 to 0.999999999...
// create random number between 0 to 10
const randNum = Math.floor((Math.random() * 11))
let randomNumRoundToFloor = Math.floor( Math.random() * 10 );
console.log(randNum, randomNumRoundToFloor)
console.log(Math.E, Math.cos(60))

// if the string length is too big it does not fit in one line. We can use the backslash character (\) at the end of each line to indicate that the string will continue on the next line.

// Template Literals(Template Strings)
let a = 3, b = 5;
console.log(`the sum of ${a} and ${b} is ${a + b}: ${8 == a + b}`)

// String methods
let js = "JavaScript"
console.log(js.length)
console.log(js[js.length - 1])
console.log(js.toUpperCase)
console.log(js.toLowerCase)
// substr(): It takes two arguments, the starting index and number of characters to slice.
console.log(js.substr(4, 6))
// substring(): It takes two arguments, the starting index and the stopping index but it doesn't include the stopping index.
console.log(js.substring(0, 4))
// split(): The split method splits a string at a specified place.
console.log(js.split(''))
console.log(js.split('S'))
// trim(): Removes trailing space in the beginning or the end of a string.
let s = ' 30 days of js  '
console.log(s.trim(' '))
let n = '   rapt'
console.log(n.trim())
// includes(): It takes a substring argument and it check if substring argument exists in the string. includes() returns a boolean.
if(js.includes('Java')) {
    console.log("true!")
}
// replace(): takes to parameter the old substring and new substring.
let country = 'china'
console.log(country.replace('c', 'C'))
// charAt(): Takes index and it returns the value at that index
console.log(js.charAt(js.length - 1))
// charCodeAt(): Takes index and it returns char code(ASCII number) of the value at that index
console.log(js.charCodeAt(2))
// indexOf(): Takes takes a substring and if the substring exists in a string it returns the first position of the substring if does not exist it returns -1
console.log(js.indexOf('java'))
console.log(js.indexOf('Sc'))
// lastIndexOf(): Takes takes a substring and if the substring exists in a string it returns the last position of the substring if it does not exist it returns -1
console.log(s.lastIndexOf(s.length - 1))
// concat(): it takes many substrings and creates concatenation.
console.log(s.concat('is great!'))
// startsWith: it takes a substring as an argument and it checks if the string starts with that specified substring. It returns a boolean(true or false).
let string = 'China'
console.log(string.startsWith('c'));
// endsWith: it takes a substring as an argument and it checks if the string starts with that specified substring. It returns a boolean(true or false).
let s1 = 'in the world'
console.log(s1.endsWith(' world'))
// search: it takes a substring as an argument and it returns the index of the first match.
console.log(s1.search("world"))
// match: it takes a substring or regular expression pattern as an argument and it returns an array if there is match if not it returns null. Let us see how a regular expression pattern looks like. It starts with / sign and ends with / sign.
let s2 = 'love'
let patternOne = /love/
let patternTwo = /love/gi  // g-means to search in the whole text, i - case insensitive
console.log(s2.match(patternOne), s2.match(patternTwo))

