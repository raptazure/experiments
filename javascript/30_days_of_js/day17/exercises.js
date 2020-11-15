localStorage.setItem('firstName', 'Liu');
console.log(localStorage);

localStorage.setItem('age', 18);
console.log(localStorage);

const student = {
  firstName: 'meow',
  lastName: 'www',
  age: 18,
  skills: ['JavaScript', 'TypeScript'],
  country: 'China',
  keys: '3e4f5d'
};

const stuText = JSON.stringify(student, undefined, 2);
localStorage.setItem("student", stuText);

personAccount = {
  firstName: 'ww',
  lastName: 'www',
  incomes: {
    233: 'daily',
    20000: 'yearly'
  },
  totalIncome() {
    const income = Object.keys(this.incomes);
    console.log(income);
    const res = parseInt(income[1]) + parseInt(income[0]) * 365;
    return res;
  }
};

localStorage.setItem('personAccount', JSON.stringify(personAccount, 2));
console.log(personAccount.totalIncome());