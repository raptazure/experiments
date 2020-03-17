const isLoading: boolean = false;

const decLiteral: number = 6;
const hexLiteral: number = 0xf00d;
const binaryLiteral: number = 0b1010;
const octalLiteral: number = 0o744;

const book: string = 'a-book';

function warnUser(): void {
  alert('This is a warning message');
}

const a: void = undefined;

const sym1 = Symbol('key1');
const sym2 = Symbol('key2');
Symbol('key1') === Symbol('key1') // false

const max = Number.MAX_SAFE_INTEGER;

const max1 = max + 1
const max2 = max + 2

max1 === max2 //true

const bigMax = BigInt(Number.MAX_SAFE_INTEGER);

// Only available when targeting higher than es2020
// const bigMax1 = bigMax + 1n;
// const bigMax2 = bigMax + 2n;

max1 === max2 // false

// In ts, number and bigint are different types
declare let foo: number;
declare let bar: bigint;