const skills = ['HTML', 'CSS', 'JS', 'React', 'Node', 'Python'];
let age = 250;
let isMarried = true;
const student = {
  firstName: 'Asabeneh',
  lastName: 'Yetayehe',
  age: 250,
  isMarried: true,
  skills: ['HTML', 'CSS', 'JS', 'React', 'Node', 'Python', ]
};
const txt = `{
    "Alex": {
        "email": "alex@alex.com",
        "skills": [
            "HTML",
            "CSS",
            "JavaScript"
        ],
        "age": 20,
        "isLoggedIn": false,
        "points": 30
    },
    "Asab": {
        "email": "asab@asab.com",
        "skills": [
            "HTML",
            "CSS",
            "JavaScript",
            "Redux",
            "MongoDB",
            "Express",
            "React",
            "Node.js"
        ],
        "age": 25,
        "isLoggedIn": false,
        "points": 50
    },
    "Brook": {
        "email": "daniel@daniel.com",
        "skills": [
            "HTML",
            "CSS",
            "JavaScript",
            "React",
            "Redux"
        ],
        "age": 30,
        "isLoggedIn": true,
        "points": 50
    },
    "Daniel": {
        "email": "daniel@alex.com",
        "skills": [
            "HTML",
            "CSS",
            "JavaScript",
            "Python"
        ],
        "age": 20,
        "isLoggedIn": false,
        "points": 40
    },
    "John": {
        "email": "john@john.com",
        "skills": [
            "HTML",
            "CSS",
            "JavaScript",
            "React",
            "Redux",
            "Node.js"
        ],
        "age": 20,
        "isLoggedIn": true,
        "points": 50
    },
    "Thomas": {
        "email": "thomas@thomas.com",
        "skills": [
            "HTML",
            "CSS",
            "JavaScript",
            "React"
        ],
        "age": 20,
        "isLoggedIn": false,
        "points": 40
    },
    "Paul": {
        "email": "paul@paul.com",
        "skills": [
            "HTML",
            "CSS",
            "JavaScript",
            "MongoDB",
            "Express",
            "React",
            "Node.js"
        ],
        "age": 20,
        "isLoggedIn": false,
        "points": 40
    }
}`;

console.log(JSON.stringify(skills, undefined, 2));
console.log(JSON.stringify(age));
console.log(JSON.stringify(isMarried));
console.log(JSON.stringify(student, undefined, 2));

console.log(JSON.stringify(student, ["firstName", "lastName", "skills"], 2));

const txtObj = JSON.parse(txt, undefined, 2);
console.log(txtObj);

const frontEnd = ['HTML', 'CSS', 'JavaScript', 'React', 'Redux'];
const backEnd = ['Python', 'Node', 'Express', 'MongoDB'];

const values = Object.values(txtObj);
const keys = Object.keys(txtObj);

const findTarget = (arr) => {
  let ans = [];
  for (let i = 0; i < arr.length; i++) {
    let tmp = arr[i].skills;
    if (tmp.filter(x => backEnd.includes(x)).length !== 0 && tmp.filter(x => frontEnd.includes(x).length !== 0)) {
      ans.push(keys[i]);
    }
  }
  return ans;
}

console.log(findTarget(values));