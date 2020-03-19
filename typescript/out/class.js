"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var tslib_1 = require("tslib");
var Animal = (function () {
    function Animal() {
    }
    Animal.prototype.move = function () {
        console.log('roaming the earch...');
    };
    return Animal;
}());
var Cat = (function (_super) {
    tslib_1.__extends(Cat, _super);
    function Cat() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Cat.prototype.makeSound = function () {
        console.log('miao miao');
    };
    return Cat;
}(Animal));
var cat = new Cat();
cat.makeSound();
cat.move();
var Car = (function () {
    function Car() {
    }
    Car.prototype.run = function () {
        console.log('启动...');
    };
    return Car;
}());
var GTR = (function (_super) {
    tslib_1.__extends(GTR, _super);
    function GTR() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    GTR.prototype.init = function () {
        this.run();
    };
    return GTR;
}(Car));
var car = new Car();
var gtr = new GTR();
gtr.init();
//# sourceMappingURL=class.js.map