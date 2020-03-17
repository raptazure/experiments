declare let notSure: any;
declare let value0: any;
declare let value1: unknown;
declare let value2: any;
declare let value: unknown;
declare function getValue(value: unknown): string;
declare function error(message: string): never;
declare const empty: never[];
declare const list0: Array<number>;
declare const list1: number[];
declare let x: [string, number];
interface Tuple extends Array<string | number> {
    0: string;
    1: number;
    length: 2;
}
declare const tuple: [string, number];
declare enum Direction {
    Center = 1
}
declare let valueObj: object;
