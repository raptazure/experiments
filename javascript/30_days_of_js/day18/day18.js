// Callback

// The first parameter is err and the second is result. If the err parameter is false, there will not be error other wise it will return an error.

const doSomething = callback => {
  setTimeout(() => {
    const skills = ['HTML', 'CSS', 'JS'];
    callback('It did not go well', skills);
  }, 2000);
};

const callback = (err, res) => {
  if (err) {
    return console.log(err);
  }
  return console.log(res);
};

// It did not go well
doSomething(callback);

const doSomething0 = callback => {
  setTimeout(() => {
    const skills = ['HTML', 'CSS', 'JS'];
    callback(false, skills);
  }, 2000);
};

// [ 'HTML', 'CSS', 'JS' ]
doSomething0((err, result) => {
  if (err) {
    return console.log(err);
  }
  return console.log(result);
});

// Promise constructor: Inside the parenthesis it it takes a callback function. The promise callback function has two parameters which are the resolve and reject functions.

const promise = new Promise((resolve, reject) => {
  resolve('success');
  reject('failure');
});

const doPromise = new Promise((resolve, reject) => {
  setTimeout(() => {
    const skills = ['HTML', 'CSS', 'JS'];
    if (skills.length > 0) {
      resolve(skills);
    } else {
      reject('Something wrong has happened');
    }
  }, 2000);
});

// [ 'HTML', 'CSS', 'JS' ] 
doPromise
  .then(result => {
    console.log(result);
  })
  .catch(error => console.log(error));


const doPromise0 = new Promise((resolve, reject) => {
  setTimeout(() => {
    const skills = ['HTML', 'CSS', 'JS'];
    if (skills.indexOf('Node') !== -1) {
      resolve('fullstack developer');
    } else {
      reject('Something wrong has happened');
    }
  }, 2000);
})

// Something wrong has happened
doPromise0
  .then(result => {
    console.log(result);
  })
  .catch(error => console.log(error));

// Fetch API
// npm i node-fetch --save
const fetch = require("node-fetch");
const url = 'https://restcountries.eu/rest/v2/all';
fetch(url)
  .then(response => response.json())
  .then(data => {
    console.log(data);
  })
  .catch(err => console.log(err));

// Async and Await is an elegant way to handle promises
const square = async function (n) {
  return n * n;
};

square(2);

const fetchData = async () => {
  try {
    const response = await fetch(url);
    const contries = await response.json();
    console.log('==== async and await');
    console.log(contries);
  } catch (err) {
    console.log(err);
  }
}

fetchData();