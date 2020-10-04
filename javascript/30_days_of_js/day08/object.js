/* Creating an object */
const emptyObj = {}
const person = {
  firstName: 'Asabeneh',
  lastName: 'Yetayeh',
  age: 250,
  country: 'Finland',
  city: 'Helsinki',
  address: {
    street: 'Heitamienkatu 16',
    pobox: 2002,
    city: 'Helsinki'
  },
  skills: [
    'HTML',
    'CSS',
    'JavaScript',
    'React',
    'Node',
    'MongoDB',
    'Python',
    'D3.js'
  ],
  getFullName: function() {
    return `${this.firstName} ${this.lastName}`
  },
  'phone number': '+3584545454545'
}

/* Setting new key for an object */
person.nationality = 'Ethiopian'
person.title = 'teacher'
person.getPersonInfo = function() {
  let skillsWithoutLastSkill = this.skills
    .splice(0, this.skills.length - 1)
    .join(', ')
  let lastSkill = this.skills.splice(this.skills.length - 1)[0]

  let skills = `${skillsWithoutLastSkill}, and ${lastSkill}`
  let fullName = this.getFullName()
  let statement = `${fullName} is a ${this.title}.\nHe lives in ${this.country}.\nHe teaches ${skills}.`
  return statement
}
person.skills.push('Meteor')
person.skills.push('SasS')
console.log(person.getPersonInfo())

/* Object methods */
// Object.assign: To copy an object without modifying the original object
const copyPerson = Object.assign({}, person)
console.log(copyPerson.age)

// Object.keys: To get the keys or properties of an object as an array
const keys = Object.keys(copyPerson)
console.log(keys)     //['name', 'age', 'country', 'skills', 'address', 'getPersonInfo']
const address = Object.keys(copyPerson.address)
console.log(address)  //['street', 'pobox', 'city']

// Object.values: To get values of an object as an array
const values = Object.values(copyPerson.address)
console.log(values)

// Object.entries: To get the keys and values in an array
const entries = Object.entries(copyPerson)
console.log(entries)

// hasOwnProperty: To check if a specific key or property exist in an object
console.log(copyPerson.hasOwnProperty('name'))