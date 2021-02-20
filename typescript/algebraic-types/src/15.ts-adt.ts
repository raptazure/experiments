import { ADT, match } from "ts-adt";
import { AgeRange } from "./10.domain";
import { pipe } from "fp-ts/function";

type ConfirmRent = ADT<{
  online: { ageRange: AgeRange; email: string };
  pickup: { ageRange: AgeRange };
}>;

declare const rentSubmit1: ConfirmRent;

export const confirmation = pipe(
  rentSubmit1,
  match({
    online: ({ email }) => `payment confirmation sent to ${email}`,
    pickup: () => "you will pay at pickup",
  })
);
