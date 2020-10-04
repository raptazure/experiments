// if - else
let num = 3;
if (num > 0) {
  console.log(`${num} is a positive number`);
} else {
  console.log(`${num} is a negative number`);
}

// if - else if - else
let weather = 'sunny';
if (weather === 'rainy') {
  console.log('You need a rain coat.');
} else if (weather === 'cloudy') {
  console.log('It might be cold, you need a jacket.');
} else if (weather === 'sunny') {
  console.log('Go out freely.');
} else {
  console.log('No need for rain coat.');
}

// switch
let weather0 = 'cloudy';
switch (weather0) {
  case 'rainy':
    console.log('You need a rain coat.');
    break;
  case 'cloudy':
    console.log('It might be cold, you need a jacket.');
    break;
  case 'sunny':
    console.log('Go out freely.');
    break;
  default:
    console.log(' No need for rain coat.');
}

// ternary operators
let isRaining = true;
isRaining
  ? console.log('You need a rain coat.')
  : console.log('No need for a rain coat.');