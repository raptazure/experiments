export {}

let [first, ...rest] = [1, 2, 3, 4];
console.log(rest);
let [second] = [1, 2, 3, 4];
console.log(second);
let [ , , third, fourth] = [1, 2, 3, 4];
console.log(third);
console.log(fourth);

let tuple: [number, string, boolean] = [7, 'hello', true];
let [a, b, c, ...d] = tuple;
console.log(d);
console.log(tuple);
let[, h] = tuple;  // b: string
console.log(h);

let o = {
    e: "foo",
    f: 12,
    g: "bar"
};
// let { e, f } = o;
// ({ e, f } = { e: 'baz', f: 101 });
let { e, ...passthrough } = o;
let total = passthrough.f + ' ' + passthrough.g;
console.log(total);

let { e: newName1, f: newName2 } = o;

// Default values let you specify a default value in case a property is undefined
function keepWholeObject(wholeObject: { a: string, b?: number}) {
    let {a, b = 1001} = wholeObject;
}
// Destructuring also works in function declarations
type C = {a: string, b?: number} 
function f8({a, b}: C): void {
    // ...
}
function f9({ a = "", b = 0 } = {}): void {
    // ...
}
f9();

function f10({ a, b = 0 } = { a: "" }): void {
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
let defaults = { food: "spicy", price: "$$", ambiance: "noisy"};
let search = { ...defaults, food: "rich" };
console.log(search);
// Object spread also has a couple of other surprising limits. First, it only includes an objects’ own, 
// enumerable properties. Basically, that means you lose methods when you spread instances of an object:
class E {
    p = 12;
    m() {
        // ...
    }
}
let obj = new E();
let clone = {...obj};
console.log(clone.p);