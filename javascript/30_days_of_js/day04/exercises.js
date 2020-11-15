let a = 4;
let b = 3;
console.log(a > b ? `${a} is greater than ${b}` : `${b} is greater than ${a}`);

const now = new Date();
if(now.getDay() <= 5) {
    console.log("work day");
} else {
    console.log("weekend");
}

s = 'a';
s[0] = s[0].toUpperCase();
console.log(s);

const readline = require('readline').createInterface({
    input: process.stdin,
    output: process.stdout
});

 months = {"january":31, "february":28, "march":31, "april":30, "may":31, "june":30, 
"july":31, "august":31, "september":30, "october":31, "november":30, "december":31};


// debugger;
function titleCase(s) {
    s = s.toLowerCase();
    return s.substr(0, 1).toUpperCase() + s.substr(1).toLowerCase();
}

readline.question(`Enter month: `, (month) => {
    console.log(`${titleCase(month)} has ${months[month.toLowerCase()]} days.`);
    readline.close()
});

// const month0 = prompt("Enter month: ");
// console.log(`${titleCase(month0)} has ${months[month0.toLowerCase()]} days.`);

