"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function mixin(first, second) {
    var result = {};
    for (var id in first) {
        result[id] = first[id];
    }
    for (var id in second) {
        if (!result.hasOwnProperty(id)) {
            result[id] = second[id];
        }
    }
    return result;
}
var x = mixin({ a: "hello" }, { b: 42 });
var a = x.a;
var b = x.b;
function formatCommandline(command) {
    var line = "";
    if (typeof command === "string") {
        line = command.trim();
    }
    else {
        line = command.join(" ").trim();
    }
}
var b2 = true;
var c = "hello";
//# sourceMappingURL=intersectionTypes.js.map