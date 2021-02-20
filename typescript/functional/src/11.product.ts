import { AgeRange, PaymentMode } from "./10.domain";

export type ConfirmRent = {
  ageRange: AgeRange;
  paymentMode: PaymentMode;
  email?: string;
};

export const playload1: ConfirmRent = {
  paymentMode: "online",
  ageRange: "18-25",
  email: "test@example.com",
};
