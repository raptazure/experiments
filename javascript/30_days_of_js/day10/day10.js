/* Set */
// Creating a set
const companies = new Set()
const languages = [
  'English',
  'Finnish',
  'English',
  'French',
  'Spanish',
  'English',
  'French'
]

const setOfLanguages = new Set(languages)
console.log(setOfLanguages)

for (const language of setOfLanguages) {
  console.log(language)
}

// Adding an element to a set
companies.add('Microsoft')
companies.add('Google')

const companiesToAdd = ['Amazon', 'Oracle']
for (const company of companiesToAdd) {
  companies.add(company)
}
console.log(companies.size);

// Deleting an element
companies.delete('Amazon')
console.log(companies.size)

// Checking an element
console.log(companies.has('Google'))

// Clearing the set
companies.clear()
console.log(companies)

// Use case
const counts = []
const count = {}

for (const l of setOfLanguages) {
  const filteredLang = languages.filter(lng => lng === l)
  console.log(filteredLang) // ["English", "English", "English"]
  counts.push({
    lang: l,
    count: filteredLang.length
  })
}
console.log(counts)

// Union of sets
let a = [1, 2, 3, 4, 5]
let b = [3, 4, 5, 6]
let c = [...a, ...b]

let A = new Set(a)
let B = new Set(b)
let C = new Set(c)

console.log(C) // Set(6) {1, 2, 3, 4, 5, 6}

// Intersection of sets
let c2 = a.filter(num => B.has(num))
let C2 = new Set(c2)
console.log(C2) // Set(3) {3, 4, 5}

// Difference of sets
let c3 = a.filter(num => !B.has(num))
let C3 = new Set(c3)
console.log(C3) // Set(2) {1, 2}


/* Map */
// Creating a map
const map = new Map()
countries = [
  ['Finland', 'Helsinki'],
  ['Sweden', 'Stockholm'],
  ['Norway', 'Oslo']
]
const mapOfCountries = new Map(countries)
console.log(mapOfCountries)
console.log(mapOfCountries.size)

// Adding values to the map
mapOfCountries.set('China', 'Beijing')
console.log(mapOfCountries.size)

// Getting a value from map
console.log(mapOfCountries.get('China'))

// Checking key in map
console.log(mapOfCountries.has('Finland'))

// Getting all values from map
for (const country of mapOfCountries) {
  console.log(country)
}

for (const [country, city] of mapOfCountries) {
  console.log(country, city)
}