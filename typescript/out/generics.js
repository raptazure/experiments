"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function returnItem(para) {
    return para;
}
function swap(tuple) {
    return [tuple[1], tuple[0]];
}
swap([7, 'seven']);
function getArrayLength(arg) {
    console.log(arg.length);
    return arg;
}
var returnItem0 = function (para) { return para; };
var Stack = (function () {
    function Stack() {
        this.arr = [];
    }
    Stack.prototype.push = function (item) {
        this.arr.push(item);
    };
    Stack.prototype.pop = function () {
        this.arr.pop();
    };
    return Stack;
}());
var Stack1 = (function () {
    function Stack1() {
        this.arr = [];
    }
    Stack1.prototype.push = function (item) {
        this.arr.push(item);
    };
    Stack1.prototype.pop = function () {
        this.arr.pop();
    };
    return Stack1;
}());
var stack1 = new Stack1();
function getValue(obj, key) {
    return obj[key];
}
var a = {
    name: 'xiaomuzhu',
    id: 1
};
getValue(a, 'id');
var Demo1 = (function () {
    function Demo1() {
    }
    Demo1.prototype.useT = function () {
        this.genericProperty.doSomething();
        this.genericProperty.doSomethingElse();
    };
    return Demo1;
}());
var Demo2 = (function () {
    function Demo2() {
    }
    Demo2.prototype.useT = function () {
        this.genericProperty.doSomething();
        this.genericProperty.doSomethingElse();
    };
    return Demo2;
}());
function factory(type) {
    return new type();
}
//# sourceMappingURL=generics.js.map