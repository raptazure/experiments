declare enum Direction0 {
    Up = 0,
    Down = 1,
    Left = 2,
    Right = 3
}
declare enum Direction1 {
    Up = 10,
    Down = 11,
    Left = 12,
    Right = 13
}
declare enum Direction2 {
    Up = "Up",
    Down = "Down",
    Left = "Left",
    Right = "Right"
}
declare enum BooleanLikeHeterogeneousEnum {
    No = 0,
    Yes = "YES"
}
declare const enum Direction3 {
    Up = "Up",
    Down = "Down",
    Left = "Left",
    Right = "Right"
}
declare const a0 = Direction3.Up;
declare enum Direction4 {
    Up = 0,
    Down = 1,
    Left = 2,
    Right = 3
}
declare const a1 = 0;
declare type c = 0;
declare let b: c;
declare enum Direction5 {
    Up = 0,
    Down = 1,
    Left = 2,
    Right = 3
}
declare let a2: Direction5;
declare enum Animal {
    Dog = 0,
    Cat = 1
}
declare enum Direction6 {
    Up = "Up",
    Down = "Down",
    Left = "Left",
    Right = "Right"
}
declare enum Direction6 {
    Center = 1
}
declare enum Month {
    January = 0,
    February = 1,
    March = 2,
    April = 3,
    May = 4,
    June = 5,
    July = 6,
    August = 7,
    September = 8,
    October = 9,
    November = 10,
    December = 11
}
declare function isSummer(month: Month): boolean;
declare namespace Month {
    function isSummer(month: Month): boolean;
}
