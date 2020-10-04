// try: wrap suspicious code that may throw an error in try block.The try statement allows us to define a block of code to be tested for errors while it is being executed.
// catch: write code to do something in catch block when an error occurs. The catch block can have parameters that will give you error information. Catch block is used to log an error or display specific messages to the user.
// finally: finally block will always be executed regardless of the occurrence of an error. The finally block can be used to complete the remaining task or reset variables that might have changed before error occurred in try block.

try {
  let lastName = 'Yetayeh';
  let fullName = fistName + ' ' + lastName;
} catch {
  console.error('err');
}

// This parameter (err) is an object and it has name and message keys.
try {
  let lastName = 'Yetayeh';
  let fullName = fistName + ' ' + lastName;
} catch (err) {
  console.log('Name of the error:', err.name);
  console.log('Error message:', err.message);
} finally {
  console.log('In any case I will be executed');
}

// throw: the throw statement allows us to create a custom error. We can through a string, number, boolean or an object. Use the throw statement to throw an exception. When you throw an exception, expression specifies the value of the exception.

const readline = require('readline');
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});
const throwErroExampleFun = () => {
  rl.question('please input a number:', x => {
    try {
      if (x == '') throw 'empty';
      if (isNaN(x)) throw 'not a number';
      x = Number(x);
      if (x < 5) throw 'too low';
      if (x > 10) throw 'too high';
    } catch (err) {
      console.log(err);
      rl.close();
    }
  });
};
throwErroExampleFun();

// ReferenceError: An illegal reference has occurred. A ReferenceError is thrown if we use a variable that has not been declared.
// SyntaxError: A syntax error has occurred
// TypeError: A type error has occurred