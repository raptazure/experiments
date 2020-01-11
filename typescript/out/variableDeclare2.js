"use strict";
var __rest = (this && this.__rest) || function (s, e) {
    var t = {};
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === "function")
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) if (e.indexOf(p[i]) < 0)
            t[p[i]] = s[p[i]];
    return t;
};
Object.defineProperty(exports, "__esModule", { value: true });
let [first, ...rest] = [1, 2, 3, 4];
console.log(rest);
let [second] = [1, 2, 3, 4];
console.log(second);
let [, , third, fourth] = [1, 2, 3, 4];
console.log(third);
console.log(fourth);
let tuple = [7, 'hello', true];
let [a, b, c, ...d] = tuple;
console.log(d);
console.log(tuple);
let [, h] = tuple; // b: string
console.log(h);
let o = {
    e: "foo",
    f: 12,
    g: "bar"
};
// let { e, f } = o;
// ({ e, f } = { e: 'baz', f: 101 });
let { e } = o, passthrough = __rest(o, ["e"]);
let total = passthrough.f + ' ' + passthrough.g;
console.log(total);
let { e: newName1, f: newName2 } = o;
// Default values let you specify a default value in case a property is undefined
function keepWholeObject(wholeObject) {
    let { a, b = 1001 } = wholeObject;
}
function f8({ a, b }) {
    // ...
}
function f9({ a = "", b = 0 } = {}) {
    // ...
}
f9();
function f10({ a, b = 0 } = { a: "" }) {
    // ...
}
f10({ a: "yes" }); // ok, default b = 0
f10(); // ok, default to { a: "" }, which then defaults b = 0
// The spread operator is the opposite of destructuring. It allows you to 
// spread an array into another array, or an object into another object
let _1st = [1, 2];
let _2nd = [3, 4];
let bothPlus = [0, ..._1st, ..._2nd, 5];
console.log(bothPlus);
let defaults = { food: "spicy", price: "$$", ambiance: "noisy" };
let search = Object.assign({}, defaults, { food: "rich" });
console.log(search);
// Object spread also has a couple of other surprising limits. First, it only includes an objects’ own, 
// enumerable properties. Basically, that means you lose methods when you spread instances of an object:
class E {
    constructor() {
        this.p = 12;
    }
    m() {
        // ...
    }
}
let obj = new E();
let clone = Object.assign({}, obj);
console.log(clone.p);
//# sourceMappingURL=variableDeclare2.js.map