interface Speak {
    (words: string): string;
}
interface User {
    name: string;
    age?: number;
    readonly isMale: boolean;
    say: (words: string) => string;
    speak: Speak;
}
declare const getUserName: (user: User) => string;
interface Config {
    width?: number;
}
declare function CalculateAreas(config: Config): {
    area: number;
};
interface Phone {
    [name: string]: string;
}
interface User0 {
    name: string;
    age?: number;
    readonly isMale: boolean;
    say: () => string;
    phone: Phone;
}
interface VIPUser extends User {
    broadcast: () => void;
}
