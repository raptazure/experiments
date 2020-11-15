const countries = ['Finland', 'Sweden', 'Norway']
const numSet = new Set()
for (let i = 0; i <= 10; i++) {
  numSet.add(i)
}

numSet.delete(10)
console.log(numSet)
numSet.clear()

const strSet = new Set()
const arr = ['h', 'e', 'l', 'l', 'o', 'w']
for (const str of arr) {
  strSet.add(str)
}
console.log(strSet)

const countriesAndLen = []
for (const country of countries) {
  countriesAndLen.push([country, country.length])
}

const countryMap = new Map(countriesAndLen)
console.log(countryMap)

const a = [4, 5, 8, 9]
const b = [3, 4, 5, 7]
const c = [...a, ...b]
let B = new Set(b)
let C = new Set(c)
console.log(C)

let c2 = a.filter(num => B.has(num))
let C2 = new Set(c2)
console.log(C2)

let c3 = a.filter(num => !B.has(num))
let C3 = new Set(c3)
console.log(C3)

const countriesData = require('../day09/countries_data.js')
const mostSpokenLanguages = (arr, num) => {
  let language = {}
  for (let i = 0; i < arr.length; i++) {
    let lan = arr[i].languages
    for (let j = 0; j < lan.length; j++) {
      language[lan[j]] = 0
    }
  }
  for (let i = 0; i < arr.length; i++) {
    let lan = arr[i].languages
    for (let j = 0; j < lan.length; j++) {
      language[lan[j]]++
    }
  }
  let languageCounterList = Object.keys(language).map((key) => [key, language[key]])
  console.log(languageCounterList.length)
  languageCounterList.sort((a, b) => b[1] - a[1])
  return languageCounterList.slice(0, num + 1)
}

console.log(mostSpokenLanguages(countriesData, 10))