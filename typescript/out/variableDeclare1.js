"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function f5(condition, x) {
    if (condition) {
        let x = 100;
        return x;
    }
    return x;
}
console.log(f5(false, 0));
function sunMatrix(matrix) {
    let sum = 0;
    for (let i = 0; i < matrix.length; i++) {
        var currentRow = matrix[i];
        // shadow: it works. Compare es5 and es6 (js)
        for (let i = 0; i < currentRow.length; i++) {
            sum += currentRow[i];
        }
    }
    return sum;
}
let matrix = [[1, 2], [3, 4], [5, 6]];
console.log(sunMatrix(matrix));
// Blocked-scoped variable capturing 
function theCityThatAlwaysSleeps() {
    let getCity;
    // Each time a scope is run, it creates an “environment” of variables. That environment and its
    // captured variables can exist even after everything within its scope has finished executing.
    if (true) {
        let city = "Seattle";
        getCity = function () {
            return city;
        };
    }
    return getCity();
}
console.log(theCityThatAlwaysSleeps());
for (let i = 0; i < 10; i++) {
    setTimeout(function () { console.log(i); }, 100 * i);
}
const numLivesForCat = 9;
const kitty = {
    name: "Aurora",
    numLives: numLivesForCat,
};
let input = [1, 2];
let [first, second] = input;
// swap variables
[first, second] = [second, first];
console.log(first);
console.log(second);
function f6([first, second]) {
    console.log(first);
    console.log(second);
}
f6([1, 2]);
//# sourceMappingURL=variableDeclare1.js.map