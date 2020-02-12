// @ts-check
Object.defineProperty(exports, "__esModule", {
  value: true
});

const constants = [2.72, 3.14, 9.81, 37, 100];
const countries = ['Finland', 'Estonia', 'Sweden', 'Denmark', 'Norway'];
const rectangle = {
  width: 20,
  height: 10,
  area: 200,
  perimeter: 60
};
const users = [{
    name: 'Brook',
    scores: 75,
    skills: ['HTM', 'CSS', 'JS'],
    age: 16
  },
  {
    name: 'Alex',
    scores: 80,
    skills: ['HTM', 'CSS', 'JS'],
    age: 18
  },
  {
    name: 'David',
    scores: 75,
    skills: ['HTM', 'CSS'],
    age: 22
  },
  {
    name: 'John',
    scores: 85,
    skills: ['HTML'],
    age: 25
  },
  {
    name: 'Sara',
    scores: 95,
    skills: ['HTM', 'CSS', 'JS'],
    age: 26
  },
  {
    name: 'Martha',
    scores: 80,
    skills: ['HTM', 'CSS', 'JS'],
    age: 18
  },
  {
    name: 'Thomas',
    scores: 90,
    skills: ['HTM', 'CSS', 'JS'],
    age: 20
  }
];

const [e, pi, gravity, humanBodyTemp, waterBoilingTemp] = constants;
const [fin, est, sw, den, nor] = countries;
const {
  width,
  height,
  area,
  perimeter
} = rectangle;

for (const {
    name,
    scores,
    skills,
    age
  } of users) {
  console.log(name, age);
}

let res = [];
for (const user of users) {
  if (user.skills.length < 2) {
    res.push(user.name);
  }
}
console.log(res);

const student = ['David', ['HTM', 'CSS', 'JS', 'React'],
  [98, 85, 90, 95]
];

const [name, skills, scores] = student;
const [, , jsScore, reactScore] = scores;
console.log(name, skills, jsScore, reactScore);

const students = [
  ['David', ['HTM', 'CSS', 'JS', 'React'],
    [98, 85, 90, 95]
  ],
  ['John', ['HTM', 'CSS', 'JS', 'React'],
    [85, 80, 85, 80]
  ]
];

function convertArrayToObject(arr) {
  let res = [];
  for (const person of arr) {
    let newObj = {};
    const [name, skills, scores] = person;
    newObj.name = name;
    newObj.skills = skills;
    newObj.scores = scores;
    res.push(newObj);
  }
  return res;
}

console.log(convertArrayToObject(students));

const person = {
  name: 'David',
  age: 25,
  skills: {
    frontEnd: [{
        skill: 'HTML',
        level: 10
      },
      {
        skill: 'CSS',
        level: 8
      },
      {
        skill: 'JS',
        level: 8
      },
      {
        skill: 'React',
        level: 9
      }
    ],
    backEnd: [{
        skill: 'Node',
        level: 7
      },
      {
        skill: 'GraphQL',
        level: 8
      },
    ],
    dataBase: [{
      skill: 'MongoDB',
      level: 7.5
    }, ],
    dataScience: ['Python', 'R', 'D3.js']
  }
};

const copyPerson = {
  ...person
};
copyPerson.skills.frontEnd.push({
  skill: 'BootStrap',
  level: 8
});
copyPerson.skills.backEnd.push({
  skill: 'Express',
  level: 9
});
copyPerson.skills.dataBase.push({
  skill: 'SQL',
  level: 8
});
copyPerson.skills.dataScience.push('SQL');

console.log(JSON.stringify(copyPerson));