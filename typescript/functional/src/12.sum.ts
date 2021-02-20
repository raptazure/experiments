import { AgeRange } from "./10.domain";

export type OnlinePayment = {
  paymentMode: "online";
  ageRange: AgeRange;
  email: string;
};

export type PickupPayment = {
  paymentMode: "pickup";
  ageRange: AgeRange;
};

export type ConfirmRent = OnlinePayment | PickupPayment;

export const playload1: ConfirmRent = {
  paymentMode: "pickup",
  ageRange: "18-25",
};
