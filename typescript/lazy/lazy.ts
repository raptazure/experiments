function sum(a: number, b: number): number {
  return a + b;
}

console.log(sum(10 + 5, 20));

function lazySum(a: () => number, b: () => number): () => number {
  return () => a() + b();
}

console.log(
  lazySum(
    () => 10 + 5,
    () => 20
  )
);
