open Utils;

let getDashArray = (seconds, maxTime) => {
  let remainder = float_of_int(seconds) /. float_of_int(maxTime);
  let timeStr = remainder *. 283.0 |> int_of_float |> string_of_int;
  let strokeDasharray = timeStr ++ " 283";
  ReactDOMRe.Style.make(~strokeDasharray, ());
};

[@react.component]
let make = (~seconds, ~maxTime) =>
  <div className="timer">
    <svg className="timer-svg" viewBox="0 0 100 100">
      <g className="timer-circle">
        <circle className="timer-path-gray" cx="50" cy="50" r="45" />
        <path
          className="timer-path-green"
          d="M 50, 50
          m -45, 0
          a 45,45 0 1,0 90,0
          a 45,45 0 1,0 -90,0"
          style={getDashArray(seconds, maxTime)}
        />
      </g>
    </svg>
    <span className="timer-label"> {seconds |> formatTime |> s} </span>
  </div>;
