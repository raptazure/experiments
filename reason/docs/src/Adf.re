/* JSX */
// <MyComponent foo={bar} />
// becomes
// ([@JSX] MyComponent.createElement(~foo=bar, ~children=[], ()));
//
// <div foo={bar}> child1 child2 </div>;
// becomes
// ([@JSX] div(~foo=bar, ~children=[child1, child2], ()));
//
// <> child1 child2 </>;
// becomes
// ([@JSX] [child1, child2]);
//
// <MyComponent> foo bar </MyComponent>
// becomes
// ([@JSX] MyComponent.createElement(~children=[foo, bar], ()));
// <MyComponent> ...foo </MyComponent>
// ([@JSX] MyComponent.createElement(~children=foo, ()));
//
// <MyComponent
//   booleanAttribute=true
//   stringAttribute="string"
//   intAttribute=1
//   forcedOptional=?{Some("hello")}
//   onClick={send(handleClick)}>
//   <div> {ReasonReact.stringToElement("hello")} </div>
// </MyComponent>;

/* FFI */
external myCFunction: int => string = "theCFunction";
[@bs.val]
external getElementsByClassName: string => array(Dom.element) =
  "document.getElementsByClassName";

/* Exception */
// Exceptions are just a special kind of variant, "thrown" in exceptional cases

// let getItem = theList =>
//   if (callSomeFunctionThatThrows()) {
//     {/* return the found item here */};
//   } else {
//     raise(Not_found);
//   };

// let result =
//   try(getItem([1, 2, 3])) {
//   | Not_found => 0 /* Default value if getItem throws */
//   };

// switch (List.find(i => i === theItem, myItems)) {
// | item => print_endline(item)
// | exception Not_found => print_endline("No such item found!")
// };

/* Object */
type tesla = {. color: string};
type car('a) = {.. color: string} as 'a;

let obj: tesla = {val red = "Red"; pub color = red};

// Js.log(obj#color); /* "Red" */

type tesla' = {. drive: int => int};

let obj: tesla' = {
  val hasEnvy = ref(false);
  pub drive = speed => {
    this#enableEnvy(true);
    speed;
  };
  pri enableEnvy = envy => hasEnvy := envy
};

type teslaObj('a) = {.. drive: int => int} as 'a;

let obj:
  teslaObj({
    .
    drive: int => int,
    doYouWant: unit => bool,
  }) = {
  val hasEnvy = ref(false);
  pub drive = speed => {
    this#enableEnvy(true);
    speed;
  };
  pub doYouWant = () => hasEnvy^;
  pri enableEnvy = envy => hasEnvy := envy
};

// Promise
// Js.Promise.make: (
//   (
//     ~resolve: (. 'a) => unit,
//     ~reject: (. exn) => unit
//   ) => unit
// ) => Js.Promise.t('a);

let myPromise = Js.Promise.make((~resolve, ~reject) => resolve(. 2));

myPromise
|> Js.Promise.then_(value => {
     Js.log(value);
     Js.Promise.resolve(value + 2);
   })
|> Js.Promise.then_(value => {
     Js.log(value);
     Js.Promise.resolve(value + 3);
   })
|> Js.Promise.catch(err => {
     Js.log2("Failure!!", err);
     Js.Promise.resolve(-2);
   });
