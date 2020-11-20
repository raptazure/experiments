type phase =
  | Work
  | Play;

type state = {
  seconds: int,
  isTicking: bool,
  workTime: int,
  playTime: int,
  currentPhase: phase,
};

type action =
  | Start
  | Stop
  | Reset
  | Tick
  | TogglePhase
  | SetTime(phase, int);

let initialState = {
  seconds: 30,
  isTicking: false,
  workTime: 25,
  playTime: 5,
  currentPhase: Work,
};

let reducer = (state, action) =>
  switch (action) {
  | Start => {...state, isTicking: true}
  | Stop => {...state, isTicking: false}
  | Tick =>
    state.isTicking && state.seconds > 0
      ? {...state, seconds: state.seconds - 1} : state
  | Reset =>
    switch (state.currentPhase) {
    | Work => {...state, seconds: state.workTime * 60}
    | Play => {...state, seconds: state.playTime * 60}
    }
  | SetTime(p, i) =>
    switch (p) {
    | Work => {...state, workTime: i}
    | Play => {...state, playTime: i}
    }
  | TogglePhase =>
    switch (state.currentPhase) {
    | Work => {
        ...state,
        seconds: state.playTime * 60,
        currentPhase: Play,
        isTicking: true,
      }
    | Play => {
        ...state,
        seconds: state.workTime * 60,
        currentPhase: Work,
        isTicking: true,
      }
    }
  };
