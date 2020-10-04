// @ts-check
Object.defineProperty(exports, "__esModule", {
  value: true
});

const text = 'He earns 4000 euro from salary per month, 10000 euro annual bonus, 5500 euro online courses per month.';
const nums = text.match(/\d+/g);
console.log('The result is', (parseInt(nums[0]) + parseInt(nums[2])) * 12 + parseInt(nums[1]));

const description = 'The position of some particles on the horizontal x-axis -1, 2, -4, -3 and -1 in the negative direction, 0 at origin, 4 and 8 in the positive direction.';
const points = description.match(/[^]-?[\d+$]/g);
const sortedPoints = points.map(point => parseInt(point)).sort((a, b) => a - b);
console.log('distance: ', sortedPoints[sortedPoints.length - 1] - sortedPoints[0]);


let pattern = /^[^a-zA-Z_$]|[^0-9a-zA-Z_$]", "_"/g;
const isValidVariable = (str) => {
  if (str.match(pattern) != null) {
    return false;
  } else {
    return true;
  }
}
console.log(isValidVariable('firstName'));
console.log(isValidVariable('_1$'));
console.log(isValidVariable('1first'));
console.log(isValidVariable('@!'));

const paragraph = `I love teaching. If you do not love teaching what else can you love. I love Python if you do not love something which can give you all the capabilities to develop an application what else can you love.`;

const tenMostFreauentWords = (str) => {
  const res = [];
  const words = str.match(/\w+/g);
  const wordSet = new Set(words);
  for (const word of wordSet) {
    let count = 0;
    for (const element of words) {
      if (element === word) count++;
    }
    res.push({
      word: word,
      count: count
    });
  }
  return res.sort((a, b) => b.count - a.count).slice(0, 10);
};

console.log(tenMostFreauentWords(paragraph));

const sentence = `%I $am@% a %tea@cher%, &and& I lo%#ve %tea@ching%;. There $is nothing; &as& mo@re rewarding as educa@ting &and& @emp%o@wering peo@ple. ;I found tea@ching m%o@re interesting tha@n any other %jo@bs. %Do@es thi%s mo@tivate yo@u to be a tea@cher!?`;

let sentenceAfter = sentence.replace(/[^a-zA-Z\d\s.]/g, '');
console.log(tenMostFreauentWords(sentenceAfter).slice(0, 7));