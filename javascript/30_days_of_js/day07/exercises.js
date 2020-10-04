const findMax = (...args) => {
  let max = args[0];
  for (const arg of args) {
    if (arg > max) {
      max = arg;
    }
  }
  return max;
};
console.log(findMax(0, 10, 5));  // Math.max()

const solveQuadratic = (...args) => {
  let res = new Set();
  if (args.length == 0) {
    res.add(0);
    return res;
  }
  let ans1 = (-1 * args[1] + Math.sqrt(args[1] * args[1] - 4 * args[0] * args[2])) / 2 * args[0];
  let ans2 = (-1 * args[1] - Math.sqrt(args[1] * args[1] - 4 * args[0] * args[2])) / 2 * args[0];
  res.add(ans1);
  res.add(ans2);
  return res;
};
console.log(solveQuadratic(1, -1, -2));

function format(date) {
  let year0 = date.getFullYear();
  let month = date.getMonth() + 1;
  let date0 = date.getDate();
  let hours = date.getHours();
  let minutes = date.getMinutes();
  if(month < 10) month = '0' + month;
  if(date < 10) date = '0' + date;
  if(hours < 10) hours = '0' + hours;
  if(minutes < 10) minutes = '0' + minutes;
  return month + '/' + date0 + '/' + year0 + ' ' + hours + ':' + minutes;
}
console.log(format(new Date()));

const swapValues = (v1, v2) => {
  let tmp = v1;
  v1 = v2;
  v2 = tmp;
  return {v1, v2};
}
console.log(swapValues(3, 4));

function arrayOfHexaColors(nums) {
  let ans = [];
  maxIndex = hexa.length - 1;
  for (let i = 0; i < nums; i++) {
    let tmp = '#';
    for (let j = 0; j < 7; j++) {
      tmp += hexa[parseInt(Math.random() * maxIndex)];
    }
    ans.push(tmp);
  }
  return ans;
}

const choices = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
  'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
  'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
];

function idGenByUser(chars, ids) {
  const maxIndex = choices.length - 1;
  for (let i = 0; i < ids; i++) {
    let ans = '';
    for (let j = 0; j < chars; j++) {
      ans += choices[parseInt(Math.random() * maxIndex)];
    }
    console.log(`${i + 1}: ${ans}`);
  }
}

idGenByUser(4, 2);

const hexa = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', 'a', 'b', 'c', 'd', 'e', 'f'];

function arrayOfRgbColors(nums) {
  let ans = [];
  for (let i = 0; i < nums; i++) {
    const v1 = parseInt(Math.random() * 255);
    const v2 = parseInt(Math.random() * 255);
    const v3 = parseInt(Math.random() * 255);
    const tmp = 'rgb(' + v1.toString() + ', ' + v2.toString() + ', ' + v3.toString() + ')';
    ans.push(tmp);
  }
  return ans;
}

function generateColors(mode, nums) {
  if (mode == 'hexa') {
    console.log(arrayOfHexaColors(nums));
  }
  if(mode == 'rgb') {
    console.log(arrayOfRgbColors(nums));
  }
}

generateColors('hexa', 3);
generateColors('rgb', 2);

function shuffle(arr) {
  for (let i = arr.length - 1; i >= 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [arr[i], arr[j]] = [arr[j], arr[i]];
  }
  return arr;
}

console.log(shuffle([1,2,3,4,5]));

const isPrime = (num) => {
  if (num < 2) {
    return false;
  }
  for (let i = 0; i <= Math.sqrt(num); i++) {
    if (num % i == 0) {
      return false;
    }
  }
  return true;
};

console.log(isPrime(7));

const isUnique = function(arr) {
  let arrSet = new Set(arr);
  if (arrSet.size == arr.length) {
    return true;
  }
  return false;
}

console.log(isUnique([1,2,3,4,4]));

let sameType = (function(arr) {
  let base = typeof arr[0];
  for (const element of arr) {
    if (typeof element != base) {
      console.log('not the same');
      return;
    }
  }
  console.log('the same');
  return;
})([1,2,3,'s']);

let pattern = /^[^a-zA-Z_$]|[^0-9a-zA-Z_$]", "_"/g;
const nameChecker = (str) => {
  if (str.match(pattern) != null) {
    console.log('invalid');
  } else {
    console.log('valid');
  }
}
nameChecker('123');
nameChecker('_$234t');
nameChecker('**&&w');

function sevenRandomNums() {
  let res = [];
  while (res.length < 7) {
    let tmp = Math.floor(Math.random() * 10);
    if (!res.includes(tmp)) {
      res.push(tmp);
    }
  }
  return res;
}

console.log(sevenRandomNums());

countries = require('../day05/countries.js');
let reverseCountries = (function(arr) {
  return arr.reverse();
})(countries);
console.log(countries.slice(0, 3));
