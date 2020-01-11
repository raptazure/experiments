var a = 10;
function f() {
    var message = "hello, world";
    return message;
}

function f1() {
    var a = 10;
    return function g() {
        var b = a + 1;
        return b;
    }
}
var g = f1()
g();  // return '11'

function f2(shouldIntialize) {
    if(shouldIntialize) {
        var x = 10;
    }
    return x;
}
console.log(f2(true));  // return '10'
console.log(f2(false));  // return 'undefined'

// setTimeout will run a function after some number of milliseconds, but only after the for loop has stopped executing; 
// By the time the for loop has stopped executing, the value of i is 10.
for(var i = 0; i < 10; i++) {
    setTimeout(function() { console.log(i); }, 100 * i);
}

// A common work around is to use an IIFE - an Immediately Invoked Function Expression - to capture i at each iteration:
for(var i = 0; i < 10; i++) {
    (function(i) {
        // capture the current state of 'i'
        // by invoking a function with its current value
        setTimeout(function() { console.log(i); }, 100 * i);
    })(i);
}
