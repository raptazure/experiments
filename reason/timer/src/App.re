open State;

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState);
  let {seconds, currentPhase, isTicking} = state;

  React.useEffect0(() => {
    let timer = Js.Global.setInterval(() => dispatch(Tick), 1000);
    Some(() => Js.Global.clearInterval(timer));
  });

  <div className="container">
    <Header seconds dispatch currentPhase />
    <Timer
      seconds
      maxTime={
        switch (currentPhase) {
        | Work => state.workTime * 60
        | Play => state.playTime * 60
        }
      }
    />
    <TimerActions dispatch isTicking />
    <EditTime
      phase="Work"
      value={state.workTime}
      onChange={e => dispatch(SetTime(Work, e))}
    />
    <EditTime
      phase="Break"
      value={state.playTime}
      onChange={e => dispatch(SetTime(Play, e))}
    />
  </div>;
};
