[@react.component]
type state = {
  seconds: int,
  isTicking: bool,
};

type action =
  | Start
  | Stop
  | Reset
  | Tick;

let initialState = {seconds: 30, isTicking: false};

let reducer = (state, action) =>
  switch (action) {
  | Start => {...state, isTicking: true}
  | Stop => {...state, isTicking: false}
  | Reset => {...state, seconds: 30}
  | Tick => {...state, seconds: state.seconds - 1}
  };

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState);

  React.useEffect0(() => {
    let timer = Js.Global.setInterval(() => dispatch(Tick), 1000);
    Some(() => Js.Global.clearInterval(timer));
  });

  <div>
    <div> {React.string(string_of_int(state.seconds))} </div>
    <button onClick={_ => dispatch(Stop)}> {React.string("Stop")} </button>
    <button onClick={_ => dispatch(Start)}> {React.string("Start")} </button>
    <button onClick={_ => dispatch(Reset)}> {React.string("Reset")} </button>
  </div>;
};
