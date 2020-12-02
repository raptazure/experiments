Js.log("Hello, ReScript!");

// Intro
type schoolPerson =
  | Teacher
  | Director
  | Student(string);

let greeting = person =>
  switch (person) {
  | Teacher => "Hey Professor!"
  | Director => "Hello Director."
  | Student("Richard") => "Still here Ricky?"
  | Student(anyOtherName) => "Hey, " ++ anyOtherName ++ "."
  };

// Primitives
let charArray = [|'H', 'e', 'l', 'l', 'o'|];
let s = String.init(5, i => charArray[i]);

let isPowerOfTwo = x => {
  x !== 0 && x land (x - 1) === 0;
};

// Basic Structures
let listA = [1, 2, 3];
let listB = [(-1), 0, ...listA];
let listD = listA @ listB @ [7, 8];
let doubled = List.map(i => i * 2, listA);
let max =
  List.fold_left((result, item) => item > result ? item : result, 0, listA);
let reversed = List.rev(listA);
let n = List.length(listA);
let isEmpty = listA == [];
switch (listA) {
| [_, _, ..._] => "at least two"
| [_, ..._] => "at least one"
| [] => "empty"
};

let arrayA = [|1, 2, 3|];
arrayA[2] = 23;
let zeroes = Array.make(5, 0);
let squares = Array.init(5, i => i * i);
let list = Array.to_list(arrayA);
switch (arrayA) {
| [||] => "empty"
| [|_|] => "exactly one"
| [|_, _|] => "exactly two"
| _ => "at least three"
};

// Tuple
let pair = ("hello", "233");
switch (pair) {
| ("hello", name) => print_endline("Hello " ++ name)
| ("bye", name) => print_endline("Goodbye " ++ name)
| (command, _) => print_endline("Unknown command: " ++ command)
};

// Types
type pair('a, 'b) = ('a, 'b);
let x: pair(int, string) = (1, "one");
let y: pair(string, list(int)) = ("123", [1, 2, 3]);

module type Duration = {
  /* This is an opaque type. */
  type t;
  let fromSeconds: int => t;
  let fromMS: int => t;
  let add: (t, t) => t;
};

module Duration: Duration = {
  /* Duration in seconds */
  type t = int;
  let fromSeconds = value => value * 1000;
  let fromMS = value => value;
  let add = (x, y) => x + y;
};

let oneMinute = Duration.fromSeconds(60);
let twoMinutes = Duration.add(oneMinute, oneMinute);

let halfSecond = Duration.fromMS(500);
let longerThanOneMinute = Duration.add(oneMinute, halfSecond);

// Records
type person = {
  name: string,
  age: int,
};

let alice = {name: "Alice", age: 42};

print_endline("Hello " ++ alice.name);

let happyBirthday = person => {
  {...person, age: person.age + 1};
};

type mutablePerson = {
  name: string,
  mutable age: int,
};

let happyBirthday = person => {
  person.age = person.age + 1;
};

type baby = {
  name: string,
  age: int,
};

type adult = {
  name: string,
  age: int,
  job: string,
};

let hire = (baby: baby, job): adult => {
  {name: baby.name, age: baby.age, job};
};

let defaultPerson = {name: "Unknown", age: 0};

let alice = {...defaultPerson, name: "Alice"};

let makePerson = (~name="Unknown", ~age=0, ()) => {
  {name, age};
};

/* The final unit is important. It lets the compiler know you're "done". */
let alice = makePerson(~name="Alice", ());

module Person = {
  type t = {
    name: string,
    age: int,
  };
};

let flandre: Person.t = {name: "Flandre", age: 42};
let flandre' = {Person.name: "Flandre", Person.age: 42};

let getName = person => person.Person.name;

// Variants
type animal =
  | Cat
  | Dog
  | Horse
  | Snake;

let spot = Dog;

type linkedList =
  | Node(int, linkedList)
  | Empty;

let x = Node(1, Node(2, Node(3, Empty)));

let checkFirst = linkedList => {
  switch (linkedList) {
  | Node(value, _) => "First is:" ++ string_of_int(value)
  | Empty => "The list is empty"
  };
};
/* inline records */
type cat = {name: string};

type dog = {breed: string};

// type animal =
//   | Cat({name: string})
//   | Dog({breed: string});
type animal' =
  | Cat(cat)
  | Dog(dog);

let x = Cat({name: "Fluffy"});

/* tuple as arguments */
type oneArgument =
  | OneArg((int, string));

type twoArguments =
  | TwoArgs(int, string);

let x' = OneArg((1, "one"));
let y' = TwoArgs(2, "two");

/* variants must have constructors */
type t =
  | Int(int)
  | String(string);

let x'': t = Int(100);
let x'': t = String("one");

// Options and nullability
type option('value) =
  | None
  | Some('value);

let nobody: option(person) = None;

let login = () => {
  Some({name: "Alice", age: 42});
};

let remi = login();

let happyBirthday' = user => {
  switch (user) {
  | Some(person) => "Happy birthday" ++ person.name
  | None => "Please login first"
  };
};

// Functions
/* named functions */
let makeCircle = (~x, ~y, ~radius) => {};
makeCircle(~x=5, ~y=5, ~radius=10);

/* Inline function */
let y = List.map(value => value * 2, [1, 2, 3]);

let rec infiniteRecursion = () => infiniteRecursion();

/* partial application */
let add = (x, y) => x + y;
let addFive = add(5);
let eleven = addFive(6);
let twelve = addFive(7);

let divide = (a, b) => a / b;
let half = divide(_, 2);
let five = half(10);

/* Non-Mandatory Arguments */
type f = (~x: int=?, ~y: int=?, unit) => int;

let addOne = (~value=?, ()) => {
  switch (value) {
  | Some(value) => value + 1
  | None => 1
  };
};

addOne(); /* 1 */
addOne(~value=11, ()); /* 12 */

let makeCircle = (~x=0, ~y=0, ~radius=10, ()) => {};

/* Position (0, 0) with radius 10 */
makeCircle();

/* Position (10, 0) with radius 2 */
makeCircle(~x=10, ~radius=2, ());

/* final unit argument */
/* Function waiting for x and y coordinates. */
let makeUnitCircle = makeCircle(~radius=1);
let c' = makeUnitCircle(~x=5, ~y=5, ());
/* Creates a circle at (0, 0) with radius 1 */
let c = makeCircle(~radius=1, ());

let a = Some(100);
let b = None;
/* Passing Options to Non-Mandatory Arguments */
/* fn is called like `fn(~data=100, ())`
   fn(~data=?a, ());
   fn is called like `fn()`
   fn(~data=?b, ()); */

/* Referencing Previous Arguments */
let add' = (a, ~b, ~c=a + 1, ~d=b + 1, ()) => a + b + c + d;
add'(1, ~b=1, ()); /* 6 */
add'(1, ~b=1, ~c=10, ()); /* 14 */

// Recursion
let rec function1 = () => {
  function2();
}
and function2 = () => {
  function3();
}
and function3 = () => {
  function1();
};

type tree =
  | Leaf
  | Node(tree, tree);

type t' = string;
module M = {
  type nonrec t' = list(t');
};

let x: M.t' = ["Hello", "World"];

type node = {
  value: string,
  edges: list(edge),
}
and edge = {
  weight: int,
  next: node,
};

module type X = {
  let x: unit => int;
  let y: unit => int;
};

module rec A: X = {
  let x = () => 1;
  let y = () => B.y() + 1;
}
and B: X = {
  let x = () => A.x() + 2;
  let y = () => 2;
};

// Destructuring
let neko = {name: "nn", age: 495, job: "dig"};
let {name: legalName, age} = neko;

print_endline(legalName);
let hello = ({name}) => {
  print_endline("Hello " ++ name);
};

// Pattern matching
let k = 60;

let x = 3;

let y =
  switch (x) {
  | 0 => "zero"
  | 1 => "one"
  | k =>
    /* k is 3 */
    "another number " ++ string_of_int(k)
  };
/* y is "another number 3", k is still 60 */

/* variant */
type tv =
  | A
  | B(int);

let x = B(42);

let y =
  switch (x) {
  | A => "a"
  | B(0) => "b_zero"
  | B(k) => "b_" ++ string_of_int(k)
  };
/* y is now "b_42" */

let x: option(int) = Some(3);

let value =
  switch (x) {
  | None => 0
  | Some(v) => v
  };

type point = {
  x: int,
  y: int,
};

type tm =
  | A((string, int))
  | B(point)
  | C(array(int))
  | D(list(point));

let x = D([{x: 2, y: 1}]);

switch (x) {
| A(("hi", num)) => num
| B({x, y: 1}) => x
| C([|x|]) => x
| C([|2, 3, x|]) => x
| D([]) => 2
| D([{x: x1, _}, {x: x2, _}, ..._]) => x1 + x2
| _ => 42
};

// switch (x) {
// | A(("hi", num)) as v => f(v)
// | B({x: _, y: 1} as r) => g(r)
// | D([{x: _, y: 1} as r, ..._]) => g(r)
// | _ => 42
// };

let items: list(int) = [1, 2, 3, 4];

switch (items) {
| [1, 2]
| [3, 4] => "is 1,2 or 3,4"
| [5, 6 | 7, ..._] => "starts with 5, then has 6 or 7"
| _ => ""
};

type tp =
  | A(string, int)
  | B(string, (int, int))
  | C(list(point));

let x = A("hi", 2);

switch (x) {
| A("a", _) => 0
| A(_) => 1
| B(_, (i, _)) => i
| C([{x, y}, ..._]) => x + y
| _ => 2
};

switch (x) {
| A("a", _) => 0
| A(_) => 1
| B(_, (i, _)) => i
| C([{x, y}, ..._]) => x + y
| C([]) => 2
};

let data = (1, ("red", true));
let (a, (b, _) as c) = data;
/* a is 1, b is "red", c is ("red", true) */

let p = {x: 2, y: 2};

let z = 3;

let k =
  switch (p) {
  | {x, y: 0} when x == z => 0
  | {x, y: 0} when x > 3 => 1
  | {x: 2, y} when y < 10 => 2
  | {x: 2, y} when y < 2 => 3 /* never executed, but no warning */
  | _ => 4
  };
/* k is 2 */

exception IndexNegative;
exception IndexOutOfBounds;

let nth = (index, items) =>
  if (index < 0) {
    raise(IndexNegative);
  } else if (index >= Array.length(items)) {
    raise(IndexOutOfBounds);
  } else {
    items[index];
  };

let items = [|1, 2, 3|];
let y =
  try(nth(-1, items)) {
  | IndexNegative => (-1)
  | IndexOutOfBounds => (-2)
  };
/* y is -1 */

let y =
  switch (nth(-1, items)) {
  | 0 => "zero"
  | n => string_of_int(n)
  | exception IndexNegative => "index is negative"
  | exception IndexOutOfBounds => "index is too big"
  };
/* y is "index is negative" */

let f = x =>
  switch (x) {
  | Some(x) => x
  | None => ""
  };

/* equivalent */
let f =
  fun
  | Some(x) => x
  | None => "";

// Mutable bindings
type ref('a) = {mutable contents: 'a};

/*
   ref(value) creates a ref with contents as value
   x^ accesses the contents of the ref x
   x := updates the contents of the ref x
 */

let x = ref(10);
x := x^ + 10;
x := x^ + 3;
/* x^ is 23 */

/* equivalent */
let x = {contents: 10};
x.contents = x.contents + 10;
x.contents = x.contents + 3;
/* x.contents is 23 */

// Loops
let x = 1;
let y = 5;

for (i in x to y) {
  print_int(i);
  print_string(" ");
};
/* Prints: 1 2 3 4 5 */

for (i in y downto x) {
  print_int(i);
  print_string(" ");
};
/* Prints: 5 4 3 2 1 */

let i = ref(1);

while (i^ <= 5) {
  print_int(i^);
  print_string(" ");
  i := i^ + 2;
};
/* Prints: 1 3 5 */

// let break = ref(false);

// while (! break^ && condition) {
//   if (shouldBreak()) {
//     break := true;
//   } else {
//     {/* normal code */};
//   };
// };

// Modules
module Duration' = {
  type t = int;
  let fromSeconds = value => value;
  let add = (x, y) => x + y;
};

let fiveSeconds = Duration'.fromSeconds(5);
let tenSeconds = Duration'.add(fiveSeconds, fiveSeconds);

/* Files are Modules */
/* open Duration'; */

let getDuration = () => {
  open Duration';
  let five = fromSeconds(5);
  let ten = add(five, five);
  ten;
};

let ten = Duration'.(add(fromSeconds(3), fromSeconds(7)));

let timer = Duration'.{contents: fromSeconds(10)};

/* A list of durations */
let durations = Duration'.[fromSeconds(1), fromSeconds(2), fromSeconds(3)];

/* include keyword copies the contents of a module into another module. */
module BaseItem = {
  let isEnabled = () => true;
  let run = () => "Do Nothing.";
};

module Hello = {
  include BaseItem;
  let run = () => "Hello World!";
};

/* Can safely use the interface of BaseItem even if isEnabled does not change */
if (Hello.isEnabled()) {
  print_endline(Hello.run());
};

module type Durationt = {
  type t;
  let fromSeconds: int => t;
  let add: (t, t) => t;
};

module Duration'': Durationt = {
  type t = int;
  let fromSeconds = value => value;
  let add = (x, y) => x + y;

  /* The module type hides this */
  let extraStuff = () => ();
};

module type Printable = {
  type printable;
  let print: printable => string;
};

module type PrintableDuration = {
  include Durationt;
  include Printable;
};

module type PrintableDuration' = {
  include (module type of Duration);
  include Printable;
};

/* Interface files (.rei files) are module types. The file creates a module type and automatically applies it to the .re file with the same name. */

/* This would give the same behavior as using explicit modules and module types. */
/* Duration.rei */
// type t;
// let fromSeconds: int => t;
// let add: (t, t) => t;

/* Duration.re */
// type t = int;
// let fromSeconds = value => value;
// let add = (x, y) => x + y;

/* Functors are like functions that create modules. */
module type Stringable = {
  type t;
  let toString: t => string;
};

module Printer = (Item: Stringable) => {
  let print = (t: Item.t) => {
    print_endline(Item.toString(t));
  };

  let printList = (list: list(Item.t)) => {
    list |> List.map(Item.toString) |> String.concat(", ") |> print_endline;
  };
};

module IntPrinter =
  Printer({
    type t = int;
    let toString = string_of_int;
  });

IntPrinter.print(10); // 10
IntPrinter.printList([1, 2, 3]) /* 1, 2, */;
