import { double, length } from "./00.true-or-lie";
import { pipe, flow } from "fp-ts/function";

export const result0 = length(double("foo"));

// "foo" |> double |> length
export const result1 = pipe("foo", double, length);

const doubleLength = flow(double, length);

export const result2 = doubleLength("foo");
