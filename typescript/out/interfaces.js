function printLable(labeledObj) {
    console.log(labeledObj.label);
}
let myObj = { size: 10, label: "Size 10 Object" };
printLable(myObj);
function createSquare(config) {
    let newSquare = { color: "white", area: 100 };
    if (config.color) {
        newSquare.color = config.color;
    }
    if (config.width) {
        newSquare.area = config.width * config.width;
    }
    return newSquare;
}
let mySquare = createSquare({ color: "black" });
console.log(mySquare.color);
let p1 = { x: 20, y: 10 };
let a = [1, 2, 3, 4];
// TypeScript comes with a ReadonlyArray<T> type that is
// the same as Array<T> with all mutating methods removed
let ro = a;
a = ro; // Override it with a type assertion
// Variables use const whereas properties use readonly
//# sourceMappingURL=interfaces.js.map