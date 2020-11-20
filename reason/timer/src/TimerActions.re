open State;

[@react.component]
let make = (~dispatch, ~isTicking) =>
  <div className="buttons">
    <button onClick={_ => dispatch(Reset)}>
      <span className="fas fa-stop" />
    </button>
    {isTicking
       ? <button onClick={_ => dispatch(Stop)}>
           <span className="fas fa-pause" />
         </button>
       : <button onClick={_ => dispatch(Start)}>
           <span className="fas fa-play" />
         </button>}
  </div>;
