"use strict";
var isLoading = false;
var decLiteral = 6;
var hexLiteral = 0xf00d;
var binaryLiteral = 10;
var octalLiteral = 484;
var book = 'a-book';
function warnUser() {
    alert('This is a warning message');
}
var a = undefined;
var sym1 = Symbol('key1');
var sym2 = Symbol('key2');
Symbol('key1') === Symbol('key1');
var max = Number.MAX_SAFE_INTEGER;
var max1 = max + 1;
var max2 = max + 2;
max1 === max2;
var bigMax = BigInt(Number.MAX_SAFE_INTEGER);
max1 === max2;
//# sourceMappingURL=primitiveTypes.js.map