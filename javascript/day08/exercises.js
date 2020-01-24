const dog = {}
dog['color'] = 'black'
dog['name'] = 'bark'
console.log(dog)

const users = {
  Alex: {
    email: 'alex@alex.com',
    skills: ['HTML', 'CSS', 'JavaScript'],
    age: 20,
    isLoggedIn: false,
    points: 30
  },
  Asab: {
    email: 'asab@asab.com',
    skills: ['HTML', 'CSS', 'JavaScript', 'Redux', 'MongoDB', 'Express', 'React', 'Node'],
    age: 25,
    isLoggedIn: false,
    points: 50
  },
  Brook: {
    email: 'daniel@daniel.com',
    skills: ['HTML', 'CSS', 'JavaScript', 'React', 'Redux'],
    age: 30,
    isLoggedIn: true,
    points: 50
  },
  Daniel: {
    email: 'daniel@alex.com',
    skills: ['HTML', 'CSS', 'JavaScript', 'Python'],
    age: 20,
    isLoggedIn: false,
    points: 40
  },
  John: {
    email: 'john@john.com',
    skills: ['HTML', 'CSS', 'JavaScript', 'React', 'Redux', 'Node.js'],
    age: 20,
    isLoggedIn: true,
    points: 50
  },
  Thomas: {
    email: 'thomas@thomas.com',
    skills: ['HTML', 'CSS', 'JavaScript', 'React'],
    age: 20,
    isLoggedIn: false,
    points: 40
  },
  Paul: {
    email: 'paul@paul.com',
    skills: ['HTML', 'CSS', 'JavaScript', 'MongoDB', 'Express', 'React', 'Node'],
    age: 20,
    isLoggedIn: false,
    points: 40
  }
}

const values = Object.values(users)
const counter = (arr) => {
  let logged = 0,
    highPoint = 0
  for (let i = 0; i < arr.length; i++) {
    if (arr[i].isLoggedIn == true) {
      logged++
    }
    if (arr[i].points >= 50) {
      highPoint++
    }
  }
  return {
    logged,
    highPoint
  }
}

console.log(counter(values))
keys = Object.keys(users)

const findMERN = (arr) => {
  let ans = []
  for (let i = 0; i < arr.length; i++) {
    let tmp = arr[i].skills
    if (tmp.includes('MongoDB') && tmp.includes('Express') && tmp.includes('React') && tmp.includes('Node')) {
      ans.push(keys[i])
    }
  }
  return ans
}
console.log(findMERN(values))

users.raptazure = {
  email: 'raptazure@gamil.com',
  age: 18
}
console.log(users.raptazure)

const users0 = [{
    _id: 'ab12ex',
    username: 'Alex',
    email: 'alex@alex.com',
    password: '123123',
    createdAt: '08/01/2020 9:00 AM',
    isLoggedIn: false
  },
  {
    _id: 'fg12cy',
    username: 'Asab',
    email: 'asab@asab.com',
    password: '123456',
    createdAt: '08/01/2020 9:30 AM',
    isLoggedIn: true
  },
  {
    _id: 'zwf8md',
    username: 'Brook',
    email: 'brook@brook.com',
    password: '123111',
    createdAt: '08/01/2020 9:45 AM',
    isLoggedIn: true
  },
  {
    _id: 'eefamr',
    username: 'Martha',
    email: 'martha@martha.com',
    password: '123222',
    createdAt: '08/01/2020 9:50 AM',
    isLoggedIn: false
  },
  {
    _id: 'ghderc',
    username: 'Thomas',
    email: 'thomas@thomas.com',
    password: '123333',
    createdAt: '08/01/2020 10:00 AM',
    isLoggedIn: false
  }
];

const products = [{
    _id: 'eedfcf',
    name: 'mobile phone',
    description: 'Huawei Honor',
    price: 200,
    ratings: [{
        userId: 'fg12cy',
        rate: 5
      },
      {
        userId: 'zwf8md',
        rate: 4.5
      }
    ],
    likes: []
  },
  {
    _id: 'aegfal',
    name: 'Laptop',
    description: 'MacPro: System Darwin',
    price: 2500,
    ratings: [],
    likes: ['fg12cy']
  },
  {
    _id: 'hedfcg',
    name: 'TV',
    description: 'Smart TV:Procaster',
    price: 400,
    ratings: [{
      userId: 'fg12cy',
      rate: 5
    }],
    likes: ['fg12cy']
  }
]

const choices = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
  'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
  'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
];

const idGenerator = () => {
  id = ''
  for (let i = 0; i <= 6; i++) {
    id += choices[Math.floor(Math.random() * choices.length)]
  }
  return id
}

function format(date) {
  let year0 = date.getFullYear()
  let month = date.getMonth() + 1
  let date0 = date.getDate()
  let hours = date.getHours()
  let minutes = date.getMinutes()
  let dayTime = hours > 12 ? 'PM' : 'AM'
  if(month < 10) month = '0' + month
  if(date < 10) date = '0' + date
  if(hours < 10) hours = '0' + hours
  if (hours > 12) hours -= 12
  if(minutes < 10) minutes = '0' + minutes
  return month + '/' + date0 + '/' + year0 + ' ' + hours + ':' + minutes + ' ' + dayTime
}

// npm install prompt
const prompt = require('prompt')
const signUpAttributes = [
  {
    name: 'username',
    validator: /^[a-zA-Z\s\-]+$/,
    warning: 'Username is not valid, it can only contains letters, spaces, or dashes'
  },
  {
    name: 'email',
    hidden: false
  },
  {
    name: 'password',
    hidden: true
  }
]

const choiceOfSign = [
  {
    name: 'mode',
    hidden: false
  }
]

const rate = [
  {
    name: 'userId',
    hidden: false
  },
  {
    name: 'product',
    hidden: false
  },
  {
    name: 'rating',
    hidden: false
  }
]

const like = [
  {
    name: 'userId',
    hidden: false
  },
  {
    name: 'product',
    hidden: false
  }
]

userKey = Object.values(users0)
userNames = []
for (let i = 0; i < userKey.length; i++) {
  userNames.push(userKey[i].username)
}

productKey = Object.values(products)
productNames = []
for (let i = 0; i < productKey.length; i++) {
  productNames.push(productKey[i].name)
}

const signUp = () => {
  prompt.start()
  prompt.get(signUpAttributes, function (err, result) {
    if (err) {
      console.log('err')
      return 1
    } else {
      users0.push({})
      const index = users0.length - 1
      if (userNames.includes(result.username)) {
        console.log("You have already signed up,", result.username)
        return 1
      }
      users0[index].username = result.username
      users0[index].password = result.password
      users0[index].email = result.email
      users0[index]._id = idGenerator()
      users0[index].isLoggedIn = false
      users0[index].createdAt = format(new Date())
      console.log("user added:", users0[index])
    }
  })
}

const signIn = () => {
  console.log("Please enter your username and password to log in")
  prompt.start()
  prompt.get(signUpAttributes, function (err, result) {
    if (err) {
      console.log('err')
      return 1
    } else {
      targetIndex = userNames.indexOf(result.username)
      if (result.password == users0[targetIndex].password) {
        users0[targetIndex].isLoggedIn = true
        console.log("sign in success")
      } else {
        console.log("incorrect username or password")
      }
    }
  })
}

const rateProduct = () => {
  prompt.start()
  prompt.get(rate, function (err, result) {
    if (err) {
      console.log('err')
      return 1
    } else {
      targetIndex = productNames.indexOf(result.product)
      let arr = products[targetIndex].ratings
      arr.push({})
      arr[arr.length - 1].userId = result.userId
      arr[arr.length - 1].rate = result.rating
      console.log(arr)
    }
  })
}

const averageRating = () => {
  for (let i = 0; i < products.length; i++) {
    let tmpArr = products[i].ratings
    let rateSum = 0
    for (let j = 0; j < tmpArr.length; j++) {
      rateSum += tmpArr[j].rate
    }
    console.log(products[i].name, ':', rateSum)
  }
}

const likeProduct = () => {
  console.log("Please enter your id and the product you like.")
  prompt.start()
  prompt.get(like, function (err, res) {
    if (err) {
      console.log('err')
      return 1
    } else {
      targetIndex = productNames.indexOf(res.product)
      let likeList = products[targetIndex].likes
      if (!likeList.includes(res.userId)) {
        likeList.push(res.userId)
      } else {
        likeList.splice(likeList.indexOf(res.userId), 1)
      }
      console.log(products[targetIndex].likes)
    }
  })
}

console.log("Enter 'signUp', 'signIn', 'rate', 'calc ratings' or 'like' to continue")
prompt.start()
prompt.get(choiceOfSign, function (err, result) {
  if (err) {
    console.log('err')
    return 1
  } else {
    console.log(result.mode)
    if (result.mode == 'signUp') {
      signUp()
    }
    if (result.mode == 'signIn') {
      signIn()
    }
    if (result.mode == 'rate') {
      rateProduct()
    }
    if (result.mode == 'calc ratings') {
      averageRating()
    }
    if (result.mode == 'like') {
      likeProduct()
    }
  }
})
