// export declare function head<A>(as: Array<A>): A;

export function head<A>(as: Array<A>): A | undefined {
  return as[0];
}

export declare function length(a: string): number;

export declare function double(a: string): string;
