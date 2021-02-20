import { ConfirmRent, OnlinePayment, PickupPayment } from "./12.sum";

export function onlinePayment(
  data: Omit<OnlinePayment, "paymentMode">
): ConfirmRent {
  return { ...data, paymentMode: "online" };
}

export function pickupPayment(
  data: Omit<PickupPayment, "paymentMode">
): ConfirmRent {
  return { ...data, paymentMode: "pickup" };
}
