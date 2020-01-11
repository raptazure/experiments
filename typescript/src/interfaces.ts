interface LabeledValue {
    label: string;
}
function printLable (labeledObj: LabeledValue) {
    console.log(labeledObj.label);
}
let myObj = { size: 10, label: "Size 10 Object" };
printLable(myObj);

interface SquareConfig {
    // optional properties
    color?: string;
    width?: number;
}
function createSquare(config: SquareConfig): {color: string; area:number} {
    let newSquare = {color: "white", area: 100};
    if(config.color) {
        newSquare.color = config.color;
    }
    if(config.width) {
        newSquare.area = config.width * config.width;
    }
    return newSquare;
}
let mySquare = createSquare({color: "black"});
console.log(mySquare.color);

// readonly properties
interface Point {
    readonly x: number;
    readonly y: number;
}
let p1: Point = { x: 20, y: 10 };
let a: number[] = [1, 2, 3, 4];
// TypeScript comes with a ReadonlyArray<T> type that is
// the same as Array<T> with all mutating methods removed
let ro: ReadonlyArray<number> = a;
a = ro as number[];  // Override it with a type assertion
// Variables use const whereas properties use readonly


