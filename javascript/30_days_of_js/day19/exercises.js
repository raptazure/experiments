function outer() {
  console.log('outer');

  function inner() {
    console.log('inner');
  }
  return inner;
}

outer()();

function meow() {
  console.log('meow');
  let cnt = 0;

  function plus() {
    cnt++;
    return cnt;
  }

  function minus() {
    cnt--;
    return cnt;
  }

  function op() {
    return !cnt;
  }

  return {
    plus: plus(),
    mius: minus(),
    op: op()
  };
}

console.log(meow().op);

function personAccount() {
  const firstName = 'ww';
  const lastName = 'www';
  const incomes = {
    233: 'daily',
    20000: 'yearly'
  };

  function totalIncome() {
    const income = Object.keys(incomes);
    console.log(income);
    const res = parseInt(income[1]) + parseInt(income[0]) * 365;
    return res;
  }

  function accountInfo() {
    console.log('name: ' + firstName + ' ' + lastName);
  }

  return {
    income: totalIncome(),
    info: accountInfo()
  };
}

console.log(personAccount().income);
personAccount().info;