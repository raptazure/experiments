"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var a = 2333;
var ab = 2;
var ao = 76;
var ax = 0x514;
var c = "xiaomuzhu";
var d = false;
function move(distance, direction) {
}
move(1, "East");
var UserReducer = function (userAction) {
    switch (userAction.action) {
        case "delete":
            console.log(userAction.id);
            break;
        default:
            break;
    }
};
UserReducer({
    action: "delete",
    id: 111,
    info: {
        username: "meow",
    },
});
//# sourceMappingURL=discriminatedUnions.js.map