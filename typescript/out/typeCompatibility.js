"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var tslib_1 = require("tslib");
var Person = (function () {
    function Person(weight, name, born) {
        this.weight = weight;
        this.name = name;
        this.born = born;
    }
    return Person;
}());
var x;
x = new Person(120, 'www', '2000-03-15');
var x1 = function (a) { return 0; };
var y = function (b, s) { return 0; };
y = x1;
var foo = function (x, y) { };
var bar = function (x, y) { };
var bas = function () {
    var args = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        args[_i] = arguments[_i];
    }
};
foo = bar = bas;
bas = bar = foo;
var Status;
(function (Status) {
    Status[Status["Ready"] = 0] = "Ready";
    Status[Status["Waiting"] = 1] = "Waiting";
})(Status || (Status = {}));
var status = Status.Ready;
var num = 0;
status = num;
num = status;
var Animal = (function () {
    function Animal(name, numFeet) {
    }
    return Animal;
}());
var Size = (function () {
    function Size(meters) {
    }
    return Size;
}());
var a;
var s;
a = s;
s = a;
var Animal1 = (function () {
    function Animal1() {
    }
    return Animal1;
}());
var Cat = (function (_super) {
    tslib_1.__extends(Cat, _super);
    function Cat() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    return Cat;
}(Animal1));
var animal;
var cat;
animal = cat;
cat = animal;
var Size1 = (function () {
    function Size1() {
    }
    return Size1;
}());
var size;
var x2;
var y2;
x2 = y2;
y2 = x2;
var x3;
var y3;
//# sourceMappingURL=typeCompatibility.js.map