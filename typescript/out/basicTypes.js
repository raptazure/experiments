"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
let isDone = false;
let decimal = 6;
let hex = 0xf00d;
let binary = 0b1010;
let octal = 0o744;
let color = 'blue';
color = 'red';
let fullname = 'rapt';
let age = 17;
let sentence1 = `hello, my name is ${fullname}.
I will be ${age + 1} years old next month.`;
let sentence2 = 'hello, my name is ' + fullname + ',\n\n' +
    "I will be " + (age + 1) + 'years old next month.';
let list1 = [1, 2, 3];
let list2 = [1, 2, 3];
let x;
x = ['hello', 10];
console.log(x[0].substring(1));
var Color;
(function (Color) {
    Color[Color["Red"] = 0] = "Red";
    Color[Color["Green"] = 2] = "Green";
    Color[Color["Blue"] = 3] = "Blue";
})(Color || (Color = {}));
let c = Color.Green;
let colorName = Color[2];
console.log(colorName);
let notSure = 4;
notSure = 'maybe a string instead';
notSure = false;
// Avoid using Object in favor of the non-primitive object type 
// as described in our Do’s and Don’ts section.
let list3 = [1, true, "free"];
list3[1] = 100;
function warnUser() {
    console.log("this is my warning message");
}
let unusable = undefined;
unusable = null;
let u = undefined;
let n = null;
function error(message1) {
    throw new Error(message1);
}
function fail() {
    return error("something failed");
}
function infiniteLoop() {
    while (true) {
    }
}
// declare function create(o: object | null): void;
// create({ prop: 0 });
// create(null);
let someValue = "this is a string";
let strLength = someValue.length;
let someValue1 = 'this is a string';
let strLength1 = someValue.length;
//# sourceMappingURL=basicTypes.js.map