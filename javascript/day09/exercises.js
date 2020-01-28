const countries = ['Finland', 'Sweden', 'Denmark', 'Norway', 'IceLand', 'France']
const names = ['Asabeneh', 'Mathias', 'Elias', 'Brook']
const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
const products = [{
    product: 'banana',
    price: 3
  },
  {
    product: 'mango',
    price: 6
  },
  {
    product: 'potato',
    price: 7
  },
  {
    product: 'avocado',
    price: 8
  },
  {
    product: 'coffee',
    price: 10
  },
  {
    product: 'tea',
    price: 10
  }
]

countries.forEach(country => console.log(country))
const countriesToUpper = countries.map(country => country.toUpperCase())
const countriesLength = countries.map(country => country.length)
const prices = products.map(product => product.price)
console.log(prices)

const lands = countries.filter(country => country.includes('land'))
console.log(lands)

const getStringList = (arr) => {
  return arr.filter(ele => typeof ele === 'string')
}
console.log(getStringList([1, 2, 3, 'as']))

const sum = numbers.reduce((a, b) => a + b, 0)
console.log(sum)

const concatenate = countries.slice(0, countries.length - 1).reduce((a, b) => a + ', ' + b)
console.log(concatenate + ` and ${countries[countries.length - 1]} are north European countries`)

console.log(names.some(name => name.length > 7))
console.log(countries.every(country => country.includes('land')))

console.log(countries.find(country => country.length === 6))
console.log(countries.findIndex(country => country === 'Norway'))

const priceSum = products.map(product => product.price).reduce((a, b) => a + b, 0)
console.log(priceSum)

const firstLetterCounter = () => {
  const res = {}
  for (let i = 0; i < countries.length; i++) {
    res[countries[i][0]] = 0
  }
  for (let i = 0; i < countries.length; i++) {
    res[countries[i][0]]++
  }
  return res
}
console.log(firstLetterCounter())

countriesData = require('./countries_data.js')

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
  let sortedLanguages = Object.keys(language).map((key) => [key, language[key]])
  sortedLanguages.sort((a, b) => b[1] - a[1])
  return sortedLanguages.slice(0, num)
}

console.log(mostSpokenLanguages(countriesData, 5))

const mostPopulatedCountries = (arr, num) => {
  let populations = {}
  for (let i = 0; i < arr.length; i++) {
    populations[arr[i].name] = arr[i].population
  }
  let list = Object.keys(populations).map((key) => [key, populations[key]])
  list.sort((a, b) => b[1] - a[1])
  return list.slice(0, num)
}
console.log(mostPopulatedCountries(countriesData, 5))