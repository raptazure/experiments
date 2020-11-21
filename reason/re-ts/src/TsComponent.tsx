import React from "react"
import {stringifyNumber} from './reason/StringFunctions.gen'

type Props = {
  headerText: string;
  num: number;
}

const TsComponent = (props: Props) => {
  const stringifiedNumber = stringifyNumber(props.num);
  return (
    <div className="tsdiv">
      <h1>{props.headerText}</h1>
      <span>{stringifiedNumber}</span>
    </div>
  )
}

export default TsComponent