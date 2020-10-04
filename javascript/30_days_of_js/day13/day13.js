/* console.log() */
// Showing output on browser console
console.log('30 Days of JavaScript');

// Substitution
console.log('%d %s of JavaScript', 30, 'Days');

// CSS: style logging message using CSS
console.log('%c30 Days Of JavaScript', 'color:green') // log output is green
console.log(
  '%c30 Days%c %cOf%c %cJavaScript%c',
  'color:green',
  '',
  'color:red',
  '',
  'color:yellow'
); // log output - green red and yellow text

/* console.warn() */
// For instance to inform or warn deprecation of version of a package or bad practices. 
console.warn('This is a warning');

/* console.error() */
console.error('This is an error message');

/* console.table() */
// The console.table() takes one required argument data, which must be an array or an object, and one additional optional parameter columns.

// A table with two columns: an index column to display the index and value column to display the names
const names = ['Asabeneh', 'Brook', 'David', 'John'];
console.table(names);

const countries = [
  ['Finland', 'Helsinki'],
  ['Sweden', 'Stockholm'],
  ['Norway', 'Oslo']
];
console.table(countries);

// A table with two columns: an index column containing the keys and a value column contain the values of the object.
const user = {
  name: 'Asabeneh',
  title: 'Programmer',
  country: 'Finland',
  city: 'Helsinki',
  age: 250
};
console.table(user);

const users = [{
    name: 'Asabeneh',
    title: 'Programmer',
    country: 'Finland',
    city: 'Helsinki',
    age: 250
  },
  {
    name: 'Eyob',
    title: 'Teacher',
    country: 'Sweden',
    city: 'London',
    age: 25
  },
  {
    name: 'Asab',
    title: 'Instructor',
    country: 'Norway',
    city: 'Oslo',
    age: 22
  },
  {
    name: 'Matias',
    title: 'Developer',
    country: 'Denmark',
    city: 'Copenhagen',
    age: 28
  }
];
console.table(users);

/* console.time() */
// Starts a timer you can use to track how long an operation takes. You give each timer a unique name, and may have up to 10,000 timers running on a given page. When you call console.timeEnd() with the same name, the browser will output the time, in milliseconds, that elapsed since the timer was started.
const countries = [
  ['Finland', 'Helsinki'],
  ['Sweden', 'Stockholm'],
  ['Norway', 'Oslo']
];

console.time('Regular for loop');
for (let i = 0; i < countries.length; i++) {
  console.log(countries[i][0], countries[i][1]);
}
console.timeEnd('Regular for loop');

console.time('for of loop');
for (const [name, city] of countries) {
  console.log(name, city);
}
console.timeEnd('for of loop');

console.time('forEach loop');
countries.forEach(([name, city]) => {
  console.log(name, city);
})
console.timeEnd('forEach loop');

// Finland Helsinki
// Sweden Stockholm
// Norway Oslo
// Regular for loop: 0.142822265625ms
// Finland Helsinki
// Sweden Stockholm
// Norway Oslo
// for of loop: 0.09521484375ms
// Finland Helsinki
// Sweden Stockholm
// Norway Oslo
// forEach loop: 0.156005859375ms

/* console.info() */
console.info('meow~');

/* console.assert() */
// Writes an error message to the console if the assertion is false. If the assertion is true, nothing happens. The first parameter is an assertion expression. If this expression is false, an Assertion failed error message will be displayed.
console.assert(4 > 3, '4 is greater than 3'); // no result
console.assert(3 > 4, '3 is not greater than 4'); // Assertion failed: 3 is not greater than 4

for (let i = 0; i <= 10; i += 1) {
  let errorMessage = `${i} is not even`;
  console.log('the # is ' + i);
  console.assert(i % 2 === 0, {
    number: i,
    errorMessage: errorMessage
  });
}

/* console.group() */
// Helps to group different log groups
const names = ['Asabeneh', 'Brook', 'David', 'John'];
const countries = [
  ['Finland', 'Helsinki'],
  ['Sweden', 'Stockholm'],
  ['Norway', 'Oslo']
];
const user = {
  name: 'Asabeneh',
  title: 'Programmer',
  country: 'Finland',
  city: 'Helsinki',
  age: 250
};
const users = [{
    name: 'Asabeneh',
    title: 'Programmer',
    country: 'Finland',
    city: 'Helsinki',
    age: 250
  },
  {
    name: 'Eyob',
    title: 'Teacher',
    country: 'Sweden',
    city: 'London',
    age: 25
  },
  {
    name: 'Asab',
    title: 'Instructor',
    country: 'Norway',
    city: 'Oslo',
    age: 22
  },
  {
    name: 'Matias',
    title: 'Developer',
    country: 'Denmark',
    city: 'Copenhagen',
    age: 28
  }
];

console.group('Names');
console.log(names);
console.groupEnd();

console.group('Countries');
console.log(countries);
console.groupEnd();

console.group('Users');
console.log(user);
console.log(users);
console.groupEnd();

/* console.count() */
// Prints the number of time this console.count() is called. It takes a string label parameter. It is very helpful to count the number of times a function is called.
const func = () => {
  console.count('Function has been called');
}
func();
func();
func();

/* console.clear() */
// Cleans the browser console.