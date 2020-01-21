console.log(parseInt('9.8') === 10);
console.log(!('python'.includes('on') && 'dragon'.includes('on')));

const base = prompt("Enter base");
const height = prompt("Enter height");
console.log(`The area of the triangle is ${0.5 * base * height}`);

const name = prompt("Enter your name");
if(name.length > 7) {
    alert("Your name is long.");
} else {
    alert("Your name is short.");
}

const birth = prompt("Enter birth year: ");
if(2020 - birth >= 18) {
    alert(`You are ${2020-birth} and you are old enough to drive`);
} else {
    alert(`You are ${2020-birth} and you will be allowed to drive atfer ${18-(2020-birth)} years`);
}

function format(date) {
    let year0 = date.getFullYear();
    let month = date.getMonth() + 1;
    let date0 = date.getDate();
    let hours = date.getHours();
    let minutes = date.getMinutes();
    if(month < 10) month = '0' + month;
    if(date < 10) date = '0' + date;
    if(hours < 10) hours = '0' + hours;
    if(minutes < 10) minutes = '0' + minutes;
    return year0 + '-' + month + '-' + date0 + ' ' + hours + ':' + minutes;
}
console.log(format(new Date()));