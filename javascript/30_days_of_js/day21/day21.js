// HTML document is structured as a JavaScript Object. Every HTML element has a different properties which can help to manipulate it. It is possible to get, create, append or remove HTML elements using JavaScript. Selecting HTML element using JavaScript is similar to selecting using CSS. To select an HTML element, we use tag name, id, class name or other attributes.

/* Getting Element */

// getElementsByTagName(): takes a tag name as a string parameter returns an HTMLCollection object. An HTMLCollection is an array like object of HTML elements. The length property provides the size of the collection. Whenever we use this method we access the individual elements using index or after loop through each individual items. An HTMLCollection does not support all array methods therefore we should use regular for loop instead of forEach.

const allTitles = document.getElementsByTagName('h1');
console.log(allTitles);
console.log(allTitles.length);
for (let i = 0; i < allTitles.length; i++) {
  console.log(allTitles[i]);
}

// getElementsByClassName(): returns an HTMLCollection object. An HTMLCollection is an array like list of HTML elements. The length property provides the size of the collection. It is possible to loop through all the HTMLCollection elements.

const allTitlesByClass = document.getElementsByClassName('title');
console.log(allTitlesByClass);
console.log(allTitlesByClass.length);
for (let i = 0; i < allTitlesByClass.length; i++) {
  console.log(allTitlesByClass[i]);
}

// getElementsById(): targets a single HTML element. We pass the id without # as an argument.

let firstTitle = document.getElementById('first-title');
console.log(firstTitle); // <h1>First Title</h1>

// querySelector: can be used to select HTML element by its tag name, id or class. If the tag name is used it selects only the first element.

let firstTitle0 = document.querySelector('h1'); // select the first available h1 element
let firstTitle1 = document.querySelector('#first-title'); // select id with first-title
let firstTitle2 = document.querySelector('.title'); // select the first available h1 element with class title

// querySelectorAll: can be used to select html element by its tag name or class. It return a nodeList which is an array like object which support array methods. We can use for loop or forEach to loop through each nodeList elements.

const allTitlesQuery = document.querySelectorAll('h1');

console.log(allTitlesQuery.length); // 4
for (let i = 0; i < allTitlesQuery.length; i++) {
  console.log(allTitlesQuery[i]);
}

allTitlesQuery.forEach(title => console.log(title));
const allTitlesQuery0 = document.querySelectorAll('.title'); // the same goes for selecting using class

/* Adding attribute */

// Adding attribute using setAttribute: set any html attribute. It takes two parameters the type of the attribute and the name of the attribute.
const titles = document.querySelectorAll('h1');
titles[3].setAttribute('class', 'title');
titles[3].setAttribute('id', 'fourth-title');

//another way to setting an attribute
titles[3].className = 'title';
titles[3].id = 'fourth-title';

// Adding class using classList: The class list method is a good method to append additional class. It does not override the original class if a class exists rather it adds additional class for the element.

titles[3].classList.add('title', 'header-title');

// Removing class using remove
titles[3].classList.remove('title', 'header-title');

// Adding Text to HTML element

// The textContent property is used to add text to an HTML element.
titles[3].textContent = 'Fourth Title';

// We use innerHTML property when we want to replace or add a completely new children content to a parent element.
titles[3].innerHTML = '4th-title';

// The properties of css when we use it in JavaScript is going to be a camelCase. The following CSS properties change from background-color to backgroundColor, font-size to fontSize, font-family to fontFamily, margin-bottom to marginBottom.

// Adding Style Color
titles.forEach((title, i) => {
  title.style.fontSize = '24px';
  if (i % 2 === 0) {
    title.style.color = 'green';
  } else {
    title.style.color = 'red';
  }
});

// Adding Style Background Color
titles.forEach((title, i) => {
  if (i % 2 === 0) {
    title.style.backgroundColor = 'red';
  } else {
    title.style.backgroundColor = 'green';
  }
});