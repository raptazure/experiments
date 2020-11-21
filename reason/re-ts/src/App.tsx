import React from "react";
import TsComponent from "./TsComponent";
import {ReasonComponent} from './reason/ReasonComponent.gen'

import "./App.css";

const App: React.FC = () => {
  return (
    <div className="App">
      <header className="App-header">
        <TsComponent headerText="TsComponent" num={2}/>
        <ReasonComponent headerText="ReasonComponent" num={1}/>
      </header>
    </div>
  );
};

export default App;
