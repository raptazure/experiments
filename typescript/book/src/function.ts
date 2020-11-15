export {}
// const add: (a: number, b: number) => number = (a: number, b: number) => a + b;
const add = (a: number, b: number) => a + b

const addSelected = (a: number, b?: number) => a + (b ? b : 0)
const addDefault = (a: number, b = 10) => a + b
const addRest = (a: number, ...rest: number[]) => rest.reduce(((a, b) => a + b), a)

// overload
interface Direction {
  top: number,
  bottom?: number,
  left?: number,
  right?: number
}
function assigned(all: number): Direction
function assigned(topAndBottom: number, leftAndRight: number): Direction
function assigned(top: number, right: number, bottom: number, left: number): Direction

function assigned (a: number, b?: number, c?: number, d?: number) {
  if (b === undefined && c === undefined && d === undefined) {
    b = c = d = a
  } else if (c === undefined && d === undefined) {
    c = a
    d = b
  }
  return {
    top: a,
    right: b,
    bottom: c,
    left: d
  }
}

assigned(1)
assigned(1,2)
// assigned(1,2,3)
assigned(1,2,3,4)