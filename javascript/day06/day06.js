// for loop
const countries = ['Finland', 'Sweden', 'Denmark', 'orway', 'Iceland'];
const newArr = [];
for (let i = 0; i < countries.length; i++) {
  newArr.push(countries[i].toUpperCase());
}
console.log(newArr);

// while loop
let i = 0
while (i <= 5) {
  process.stdout.write(`${i} `);
  i++;
}  // 0 1 2 3 4 5
console.log('');

i = 0;
do {
  process.stdout.write(`${i} `);
  i++;
} while (i <= 5)   // 0 1 2 3 4 5
console.log('');

// for of loop
const webTechs = [
  'HTML',
  'CSS',
  'JavaScript',
  'React',
  'Redux',
  'Node',
  'MongoDB'
]
for (const tech of webTechs) {
  process.stdout.write(tech[0] + ' ');
}   // H C J R N M
