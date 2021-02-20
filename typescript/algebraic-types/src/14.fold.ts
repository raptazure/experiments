import { ConfirmRent, OnlinePayment, PickupPayment } from "./12.sum";
import { identity, pipe } from "fp-ts/function";

function fold<R>(match: {
  online: (onlinePayment: OnlinePayment) => R;
  pickup: (pickupPayment: PickupPayment) => R;
}): (rentSubmit: ConfirmRent) => R {
  return (rentSubmit) => {
    switch (rentSubmit.paymentMode) {
      case "online":
        return match.online(rentSubmit);
      case "pickup":
        return match.pickup(rentSubmit);
    }
  };
}

export function renderConfirmation(rentSubmit: ConfirmRent): string {
  return pipe(
    rentSubmit,
    fold({
      online: ({ email }) => `payment confirmation sent to ${email}`,
      pickup: () => "you will pay at pickup",
    })
  );
}

import * as option from "fp-ts/Option";

import { AgeRange } from "./10.domain";

declare const ageRange: AgeRange | undefined | null;

export const result = pipe(
  ageRange,
  option.fromNullable,
  option.fold(() => "Unknown", identity)
);

export const result3 = pipe(
  ageRange,
  option.fromNullable,
  option.getOrElse(() => "Unknown")
);
