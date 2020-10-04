for (let i = 1; i <= 7; i++) {
  console.log('#'.repeat(i));
}

let arrSet = new Set();
while (arrSet.size <= 5) {
  arrSet.add(parseInt(Math.random() * 10));
}
arr = Array.from(arrSet);
console.log(arr);

const countries = [
  'Albania',
  'Bolivia',
  'Canada',
  'Denmark',
  'Ethiopia',
  'Finland',
  'Germany',
  'Hungary',
  'Ireland',
  'Japan',
  'Kenya'
];

function arrayOfArrays(arr) {
  let ans = [];
  for (let i = 0; i < arr.length; i++) {
    let tmp = [];
    tmp.push(arr[i]);
    tmp.push(arr[i].slice(0, 3).toUpperCase());
    tmp.push(arr[i].length);
    ans.push(tmp);
  }
  return ans;
}
console.log(arrayOfArrays(countries));

function checkLand(arr) {
  let ans = [];
  for (let i = 0; i < arr.length; i++) {
    if (arr[i].includes('land')) {
      ans.push(arr[i]);
    }
  }
  if (ans.length != 0) {
    console.log(ans);
  } else {
    console.log('no land');
  }
}

checkLand(countries);

mernStack = ['MongoDB', 'Express', 'React', 'Node'];
for (const tech of mernStack) {
  process.stdout.write(tech[0]);
}

const fruits =  ['banana', 'orange', 'mango', 'lemon'];
for (let i = 0; i <= fruits.length >> 1; i++) {
  let tmp = fruits[i];
  fruits[i] = fruits[fruits.length - 1 - i];
  fruits[fruits.length - 1 - i] = tmp;
}
console.log(fruits);

let modifiedCountries = [];
for (let i = countries.length - 1; i >= 0; i--) {
  modifiedCountries.push(countries[i].toUpperCase());
}
console.log(modifiedCountries);
