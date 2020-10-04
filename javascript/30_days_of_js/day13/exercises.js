const countries = [
  ['Finland', 'Helsinki'],
  ['Sweden', 'Stockholm'],
  ['Norway', 'Oslo']
];

const country = {
  population: 1000,
  age: 100
};

console.table(countries);
console.table(country);
console.group('countries');
console.log(countries);
console.groupEnd();

console.assert(10 > 2 * 10, '10 is bigger than 2 * 10');
console.warn('warning');
console.error('error');

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