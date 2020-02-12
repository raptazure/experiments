// @ts-check

/* Destructuring */
const numbers = [1, 2, 3];
let [numOne, numTwo, numThree] = numbers;
console.log(numOne, numTwo, numThree);

const fullStack = [
  ['HTML', 'CSS', 'JS', 'React'],
  ['Node', 'Express', 'MongoDB']
];
const [frontEnd, backEnd] = fullStack;
console.log(frontEnd);
console.log(backEnd);

// The comma helps to omit the value at specific index
const number = [1, 2, 3];
let [num1, , num3] = numbers; // 2 is omitted
console.log(num1, num3);

// We can use default value in case the value of array for that index is undefined
const names = [undefined, 'Brook', 'David'];
let [
  firstPerson = 'Asabeneh',
  secondPerson,
  thirdPerson,
  fourthPerson = 'John'
] = names;
console.log(firstPerson, secondPerson, thirdPerson, fourthPerson);

// Destructuring during iteration
const countries = [
  ['Finland', 'Helsinki'],
  ['Sweden', 'Stockholm'],
  ['Norway', 'Oslo']
];
for (const [country, city] of countries) {
  console.log(country, city);
}

for (const [first, second, third] of fullStack) {
  console.log(first, second, third);
}

// Destructuring Object
const rectangle = {
  width: 20,
  height: 10,
  area: 200
};
let {
  width,
  height,
  area,
  perimeter
} = rectangle;
console.log(area * height, perimeter);

// Renaming during structuring
let {
  width: w,
  heigh: h,
  area: a,
  perimeter: p = 60
} = rectangle;
console.log(w, h, a, p);

// Object parameter without destructuring
const calculatePerimeter = rectangle => {
  return 2 * (rectangle.width + rectangle.height);
}
console.log(calculatePerimeter(rectangle));

const person = {
  firstName: 'Asabeneh',
  lastName: 'Yetayeh',
  age: 250,
  country: 'Finland',
  job: 'Instructor and Developer',
  skills: [
    'HTML',
    'CSS',
    'JavaScript',
    'React',
    'Redux',
    'Node',
    'MongoDB',
    'Python',
    'D3.js'
  ],
  languages: ['Amharic', 'English', 'Suomi(Finnish)']
};

// Let's create a function which give information about the person object without destructuring
const getPersonInfo = obj => {
  const skills = obj.skills;
  const formattedSkills = skills.slice(0, -1).join(', ');
  const languages = obj.languages;
  const formattedLanguages = languages.slice(0, -1).join(', ');

  let personInfo = `${obj.firstName} ${obj.lastName} lives in ${obj.country}. He is  ${obj.age} years old. He is an ${obj.job}. He teaches ${formattedSkills} and ${skills[skills.length - 1]}. He speaks ${formattedLanguages} and a little bit of ${languages[2]}.`;

  return personInfo;
};

console.log(getPersonInfo(person));

// Object parameter with destructuring
const calcPeri = ({
  width,
  height
}) => 2 * (width + height)
console.log(calcPeri(rectangle));

// Lets create a function which give information about the person object with destructuring
const getPersonInfo1 = ({
  firstName,
  lastName,
  age,
  country,
  job,
  skills,
  languages
}) => {
  const formattedSkills = skills.slice(0, -1).join(', ');
  const formattedLanguages = languages.slice(0, -1).join(', ');

  let personInfo = `${firstName} ${lastName} lives in ${country}. He is ${age} years old. He is an ${job}. He teaches ${formattedSkills} and ${skills[skills.length - 1]}. He speaks ${formattedLanguages} and a little bit of ${languages[2]}.`;

  return personInfo;
}
console.log(getPersonInfo1(person));

// Destructuring object during iteration
const todoList = [{
    task: 'Prepare JS Test',
    time: '4/1/2020 8:30',
    completed: true
  },
  {
    task: 'Give JS Test',
    time: '4/1/2020 10:00',
    completed: false
  },
  {
    task: 'Assess Test Result',
    time: '4/1/2020 1:00',
    completed: false
  }
];

for (const {
    task,
    time,
    completed
  } of todoList) {
  console.log(task, time, completed);
}

/* Spread or Rest Operator */
const nums = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
let [_1, _2, _3, ...rest] = nums;
console.log(_1, _2, _3);
console.log(rest);

// Spread operator to copy array
const evens = [0, 2, 4, 6, 8, 10];
const evenNumbers = [...evens];
const odds = [1, 3, 5, 7, 9];
const oddNumbers = [...odds];
const wholeNumbers = [...evens, ...odds];
console.log(wholeNumbers);

// Spread operator to copy object
const user = {
  name: 'Asabeneh',
  title: 'Programmer',
  country: 'Finland',
  city: 'Helsinki'
};
const copiedUser = {
  ...user,
  title: 'instructor'
};
console.log(copiedUser);

// Spread operator with arrow function
const sumAllNums = (...args) => {
  let sum = 0;
  for (const num of args) {
    sum += num;
  }
  return sum;
}
console.log(sumAllNums(1, 2, 3, 4, 5));