// import User from './user.js'
import U, {
  printName as printUserName,
  printAge
} from './user.js'

// const user = new User('Bob', 11)
const user = new U('Bob', 11)
console.log(user)
printUserName(user)