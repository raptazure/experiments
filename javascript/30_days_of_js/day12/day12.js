// @ts-check

// Creating a pattern with RegEx Constructor
let pattern = 'love';
let flag = 'gi';
// let regEx = new RegEx(pattern, flag);

// Creating a pattern without RegEx Constructor
let regEx = /love/gi;

/* RegExp Object Methods */

// Testing for a match
const str = 'I love JavaScript';
const pattern1 = /love/;
console.log(pattern1.test(str));

// Array containing all of the match
const pattern2 = /love/;
console.log(str.match(pattern2));

const pattern3 = /love/g;
console.log(str.match(pattern3));

console.log(str.search(pattern3));

// Replacing a substring
const txt = 'Python is the most beautiful language that a human begin has ever created. I recommend python for a first programming language';

const matchReplaced = txt.replace(/Python|python/, 'JavaScript');
console.log(matchReplaced);
console.log(txt.replace(/Python|python/g, 'JavaScript'));
console.log(txt.replace(/Python/gi, 'JavaScript'));

const txt1 = '%I a%m te%%a%%che%r% a%n%d %%I l%o%ve te%ach%ing. T%he%re i%s n%o%th%ing as m%ore r%ewarding a%s e%duc%at%i%ng a%n%d e%m%p%ow%er%ing p%e%o%ple. I fo%und te%a%ching m%ore i%n%t%er%%es%ting t%h%an any other %jobs. D%o%es thi%s m%ot%iv%a%te %y%o%u to b%e a t%e%a%cher?';

console.log(txt1.replace(/%/g, ''));

// Square Bracket
const txt2 = 'Apple and banana are fruits. An old cliche says an apple a day a doctor way has been replaced by a banana a day keeps the doctor far far away. ';
console.log(txt2.match('[Aa]pple'));
console.log(txt2.match(/[Aa]pple/g));
console.log(txt2.match(/[Aa]pple|[Bb]anana/g));

// Escape character(\) in RegEx
const txt3 = 'This regular expression example was made in January 12,  2020.';
console.log(txt3.match(/\d/g));

// One or more times(+)
console.log(txt3.match(/\d+/g));

// Period(.)
const txt4 = 'Apple and banana are fruits';

// this square bracket means a and . means any character except new line
console.log(txt4.match(/[a]./g));
console.log(txt4.match(/[a].+/g));

// Zero or more times(*)
console.log(txt4.match(/[a].*/g));

// Zero or one times(?)
const txt5 = 'I am not sure if there is a convention how to write the word e-mail. Some people write it email others may write it as Email or E-mail.';

// ? means optional
console.log(txt5.match(/[Ee]-?mail/g));

// Quantifier in RegEx
const txt6 = 'This regular expression example was made in December 6,  2019.';
console.log(txt6.match(/\d{4}/g));
console.log(txt6.match(/\d{1,4}/g));

// Starts with
console.log(txt6.match(/^This/));

// Negation
console.log(txt6.match(/[^A-Za-z,. ]+/g));

// Exact match
let pattern4 = /^[A-Z][a-z]{3,12}$/;
console.log(pattern4.test('Asabeneh'));