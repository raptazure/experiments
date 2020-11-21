[@gentype]
let stringifyNumber = (num: int) => {
  switch (num) {
  | 0 => "Zero"
  | 1 => "One"
  | 2 => "Two"
  | _ => "Too large or too small"
  };
};
