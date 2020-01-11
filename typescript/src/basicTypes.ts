export {}

let isDone: boolean = false;
let decimal: number = 6;
let hex: number = 0xf00d;
let binary: number = 0b1010;
let octal: number = 0o744;
let color: string = 'blue';
color = 'red';

let fullname: string = 'rapt';
let age: number = 17;
let sentence1: string = `hello, my name is ${fullname}.
I will be ${age + 1} years old next month.`;
let sentence2: string = 'hello, my name is ' + fullname + ',\n\n' +
"I will be " + (age + 1) + 'years old next month.';

let list1: number[] = [1, 2, 3];
let list2: Array<number> = [1, 2, 3];

let x: [string, number];
x = ['hello', 10];
console.log(x[0].substring(1));

enum Color {Red, Green = 2, Blue}
let c: Color = Color.Green;
let colorName: string = Color[2];
console.log(colorName);

let notSure: any = 4;
notSure = 'maybe a string instead';
notSure = false;
// Avoid using Object in favor of the non-primitive object type 
// as described in our Do’s and Don’ts section.

let list3: any[] = [1, true, "free"];
list3[1] = 100;

function warnUser(): void {
    console.log("this is my warning message");
}

let unusable: void = undefined;
unusable = null;

let u: undefined = undefined;
let n: null = null;

function error(message1: string): never {
    throw new Error(message1);
}
function fail() {
    return error("something failed");
}
function infiniteLoop(): never {
    while(true) {

    }
}

// declare function create(o: object | null): void;
// create({ prop: 0 });
// create(null);

let someValue: any = "this is a string";
let strLength: number = (<string>someValue).length;

let someValue1: any = 'this is a string';
let strLength1: number = (someValue as string).length;

