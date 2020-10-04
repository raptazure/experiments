let challenge = '30 Days Of JavaScript'
console.log(challenge)
console.log(challenge.length)
console.log(challenge.toUpperCase())
console.log(challenge.substr(0, 2))
console.log(challenge.substring(3))
console.log(challenge.includes('Script'))
console.log(challenge.split(''))
console.log(challenge.split(' '))
console.log('Facebook, Google, Microsoft, Apple, IBM, Oracle, Amazon'.split(','))
console.log(challenge.replace('JavaScript', 'Python'))
console.log(challenge.charAt(15))
console.log(challenge.charCodeAt('J'))
console.log(challenge.indexOf('a'))
console.log(challenge.lastIndexOf('a'))
console.log('You cannot end a sentence with because because because is a conjunction'.search('because'))
console.log(' 30 Days Of JavaScript '.trim())
console.log(challenge.match('a'))
console.log('30 Days of'.concat(' JavaScript'))

console.log(typeof("10") == 10)
console.log(parseInt('10') == 10)
console.log(Math.round(parseFloat("9.8")) == 10)
console.log('python'.includes('on') && 'jargon'.includes('on'))
console.log(Math.random() * 50 + 50)
let s = 'JavaScript'
console.log(s[Math.round(Math.random() * 10 - 1)])

let sentence0 = 'You cannot end a sentence with because because because is a conjunction'
let patternOne = /because/g
let res = sentence0.match(patternOne)
console.log(res.length)
const sentence1 = '%I $am@% a %tea@cher%, &and& I lo%#ve %te@a@ching%;. The@re $is no@th@ing; &as& mo@re rewarding as educa@ting &and& @emp%o@weri@ng peo@ple. ;I found tea@ching m%o@re interesting tha@n any ot#her %jo@bs. %Do@es thi%s mo@tiv#ate yo@u to be a tea@cher!? %Th#is 30#Days&OfJavaScript &is al@so $the $resu@lt of &love& of tea&ching'
let sentenceAfter = sentence1.replace(/[^a-zA-Z\d\s.]/g, '')
console.log(sentenceAfter)
let tmp = sentence1.replace(/[^a-zA-Z\d\s]/g, '')
let words = tmp.split(' ')
let wordCounts = {}
for(let i = 0; i < words.length; i++) {
    wordCounts[words[i].toLowerCase()] = (wordCounts[words[i].toLowerCase()] || 0) + 1
}
let items = Object.keys(wordCounts).map(function(key) {
    return [key, wordCounts[key]]
});
items.sort(function(first, second) {
    return second[1] - first[1]
});
console.log(items.slice(0, 2))
let sentence2 = 'He earns 5000 euro from salary per month, 10000 euro annual bonus, 15000 euro online courses per month.'
let patternThree = /\d+/g
let nums = sentence2.match(patternThree)
console.log((parseInt(nums[0]) + parseInt(nums[2])) * 12 + parseInt(nums[1]))
