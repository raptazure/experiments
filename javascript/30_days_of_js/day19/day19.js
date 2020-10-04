// Closure - JavaScript allows writing function inside an outer function. We can write as many inner functions as we want. If inner function access the variables of outer function then it is called closure.

function outerFunction0() {
  let count0 = 0;

  function innerFunction() {
    count0++;
    return count0;
  }
  return innerFunction;
}

const innerFunc = outerFunction0();

console.log(innerFunc());
console.log(innerFunc());
console.log(innerFunc());

function outerFunction() {
  let count = 0;

  function plusOne() {
    count++;
    return count;
  }

  function minusOne() {
    count--;
    return count;
  }

  return {
    plusOne: plusOne(),
    minusOne: minusOne()
  };
}
const innerFuncs = outerFunction();

console.log(innerFuncs.plusOne);
console.log(innerFuncs.minusOne);