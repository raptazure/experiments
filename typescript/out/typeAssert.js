"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var person = {};
person.name = 'xiaomuzhu';
person.age = 20;
var person1 = 'xiaomuzhu';
var Person = (function () {
    function Person() {
        this.name = 'xiaomuzhu';
        this.age = 20;
    }
    return Person;
}());
var Animal = (function () {
    function Animal() {
        this.name = 'petty';
        this.color = 'pink';
    }
    return Animal;
}());
function getSometing(arg) {
    if (arg instanceof Person) {
        console.log(arg.age);
    }
    if (arg instanceof Animal) {
        console.log(arg.color);
    }
}
function getSometing1(arg) {
    if ('age' in arg) {
        console.log(arg.age);
    }
    if ('color' in arg) {
        console.log(arg.color);
    }
}
function doStuff(arg) {
    if (arg.kind === 'foo') {
        console.log(arg.foo);
    }
    else {
        console.log(arg.bar);
    }
}
//# sourceMappingURL=typeAssert.js.map