[@gentype.as "ReasonComponent"]
[@react.component]
let make = (~headerText: string, ~num: int) => {
  let stringifiedNumber = StringFunctions.stringifyNumber(num);

  <div className="reasondiv">
    <h1> {React.string(headerText)} </h1>
    <span> {React.string(stringifiedNumber)} </span>
  </div>;
};
