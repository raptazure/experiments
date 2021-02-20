import { makeADT, ofType } from "@morphic-ts/adt";
import { pipe } from "fp-ts/function";
import { OnlinePayment, PickupPayment } from "./12.sum";

const ConfirmRent = makeADT("paymentMode")({
  online: ofType<OnlinePayment>(),
  pickup: ofType<PickupPayment>(),
});

const rentSubmit = ConfirmRent.of.online({
  email: "test@example.com",
  ageRange: "18-25",
});

export const confirmation = pipe(
  rentSubmit,
  ConfirmRent.match({
    online: ({ email }) => `payment confirmation sent to ${email}`,
    pickup: () => "you will pay at pickup",
  })
);
