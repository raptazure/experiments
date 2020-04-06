"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var tslib_1 = require("tslib");
function addAge(constructor) {
    constructor.prototype.age = 18;
}
var Person = (function () {
    function Person() {
        this.name = "ww";
    }
    Person = tslib_1.__decorate([
        addAge
    ], Person);
    return Person;
}());
var person = new Person();
console.log(person.age);
function method(target, propertyKey, descriptor) {
    console.log(target);
    console.log("prop " + propertyKey);
    console.log("desc " + JSON.stringify(descriptor) + "\n\n");
    descriptor.writable = false;
}
var Person1 = (function () {
    function Person1() {
        this.name = "xiaomuzhu";
    }
    Person1.prototype.say = function () {
        return "instance method";
    };
    Person1.run = function () {
        return "static method";
    };
    tslib_1.__decorate([
        method
    ], Person1.prototype, "say", null);
    tslib_1.__decorate([
        method
    ], Person1, "run", null);
    return Person1;
}());
var xmz = new Person1();
console.log(xmz.say());
//# sourceMappingURL=decorator.js.map