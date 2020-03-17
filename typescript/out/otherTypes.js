"use strict";
var notSure = 4;
notSure = "maybe a string instead";
var value0;
value0 = true;
value0 = 1;
value0 = "Hello World";
value0 = Symbol("type");
value0 = {};
value0 = [];
var value1;
value1 = true;
value1 = 1;
value1 = "Hello World";
value1 = Symbol("type");
value1 = {};
value1 = [];
var value2;
value2.foo.bar;
value2();
new value2();
value2[0][1];
var value;
function getValue(value) {
    if (value instanceof Date) {
        return value.toISOString();
    }
    return String(value);
}
function error(message) {
    throw new Error(message);
}
var empty = [];
var list0 = [1, 2, 3];
var list1 = [1, 2, 3];
var x;
x = ['hello', 10];
var tuple = ['a', 1];
tuple.push(2);
console.log(tuple);
var Direction;
(function (Direction) {
    Direction[Direction["Center"] = 1] = "Center";
})(Direction || (Direction = {}));
var valueObj;
valueObj = Direction;
valueObj = [1];
value = [1, 'hello'];
value = {};
//# sourceMappingURL=otherTypes.js.map