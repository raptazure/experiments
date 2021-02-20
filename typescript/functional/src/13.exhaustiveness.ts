import { ConfirmRent } from "./12.sum";

export function renderConfirmation(rentSubmit: ConfirmRent): string {
  switch (rentSubmit.paymentMode) {
    case "online":
      return `Payment confirmation sent to ${rentSubmit.email}`;
    case "pickup":
      return "You will pay at pickup";
  }
}
