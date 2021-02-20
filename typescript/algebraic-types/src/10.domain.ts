export type PaymentMode = "online" | "pickup";

export type AgeRange = "18-25" | "25-27" | "27+";

export type FormState = {
  email?: string;
  ageRange?: AgeRange;
  paymentMode?: PaymentMode;
};
