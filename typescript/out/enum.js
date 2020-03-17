"use strict";
var Direction0;
(function (Direction0) {
    Direction0[Direction0["Up"] = 0] = "Up";
    Direction0[Direction0["Down"] = 1] = "Down";
    Direction0[Direction0["Left"] = 2] = "Left";
    Direction0[Direction0["Right"] = 3] = "Right";
})(Direction0 || (Direction0 = {}));
console.log(Direction0.Up === 0);
console.log(Direction0.Down === 1);
console.log(Direction0.Left === 2);
console.log(Direction0.Right === 3);
var Direction1;
(function (Direction1) {
    Direction1[Direction1["Up"] = 10] = "Up";
    Direction1[Direction1["Down"] = 11] = "Down";
    Direction1[Direction1["Left"] = 12] = "Left";
    Direction1[Direction1["Right"] = 13] = "Right";
})(Direction1 || (Direction1 = {}));
console.log(Direction1.Up, Direction1.Down, Direction1.Left, Direction1.Right);
console.log(Direction1[10], Direction1['Right']);
var Direction2;
(function (Direction2) {
    Direction2["Up"] = "Up";
    Direction2["Down"] = "Down";
    Direction2["Left"] = "Left";
    Direction2["Right"] = "Right";
})(Direction2 || (Direction2 = {}));
console.log(Direction2['Right'], Direction2.Up);
var BooleanLikeHeterogeneousEnum;
(function (BooleanLikeHeterogeneousEnum) {
    BooleanLikeHeterogeneousEnum[BooleanLikeHeterogeneousEnum["No"] = 0] = "No";
    BooleanLikeHeterogeneousEnum["Yes"] = "YES";
})(BooleanLikeHeterogeneousEnum || (BooleanLikeHeterogeneousEnum = {}));
var a0 = "Up";
var Direction4;
(function (Direction4) {
    Direction4[Direction4["Up"] = 0] = "Up";
    Direction4[Direction4["Down"] = 1] = "Down";
    Direction4[Direction4["Left"] = 2] = "Left";
    Direction4[Direction4["Right"] = 3] = "Right";
})(Direction4 || (Direction4 = {}));
var a1 = 0;
console.log(a1 === Direction4.Up);
b = Direction4.Up;
var Direction5;
(function (Direction5) {
    Direction5[Direction5["Up"] = 0] = "Up";
    Direction5[Direction5["Down"] = 1] = "Down";
    Direction5[Direction5["Left"] = 2] = "Left";
    Direction5[Direction5["Right"] = 3] = "Right";
})(Direction5 || (Direction5 = {}));
var Animal;
(function (Animal) {
    Animal[Animal["Dog"] = 0] = "Dog";
    Animal[Animal["Cat"] = 1] = "Cat";
})(Animal || (Animal = {}));
a2 = Direction5.Up;
var Direction6;
(function (Direction6) {
    Direction6["Up"] = "Up";
    Direction6["Down"] = "Down";
    Direction6["Left"] = "Left";
    Direction6["Right"] = "Right";
})(Direction6 || (Direction6 = {}));
(function (Direction6) {
    Direction6[Direction6["Center"] = 1] = "Center";
})(Direction6 || (Direction6 = {}));
var Month;
(function (Month) {
    Month[Month["January"] = 0] = "January";
    Month[Month["February"] = 1] = "February";
    Month[Month["March"] = 2] = "March";
    Month[Month["April"] = 3] = "April";
    Month[Month["May"] = 4] = "May";
    Month[Month["June"] = 5] = "June";
    Month[Month["July"] = 6] = "July";
    Month[Month["August"] = 7] = "August";
    Month[Month["September"] = 8] = "September";
    Month[Month["October"] = 9] = "October";
    Month[Month["November"] = 10] = "November";
    Month[Month["December"] = 11] = "December";
})(Month || (Month = {}));
function isSummer(month) {
    switch (month) {
        case Month.June:
        case Month.July:
        case Month.August:
            return true;
        default:
            return false;
    }
}
(function (Month) {
    function isSummer(month) {
        switch (month) {
            case Month.June:
            case Month.July:
            case Month.August:
                return true;
            default:
                return false;
        }
    }
    Month.isSummer = isSummer;
})(Month || (Month = {}));
console.log(Month.isSummer(Month.January));
//# sourceMappingURL=enum.js.map