### What is REST?

- REST stands for REpresentation State Transfer and it is a way of saying a server responds to create, read, update and delete requests in a standard way.
- The idea behind REST is to treat all server URLs as access points for the various resources on the server.
- GET: gets a list of the resource or the resource with the given ID (acts upon the entire resource or a single resource)
- POST: creates a new resource (always acts on the entire resource)
- PUT: updates the resource with the given ID (acts on a single resource)
- DELETE: deletes the resource with the given ID (acts on a single resource)
- The URL is used to represent a resource which supports creating, reading, updating and deleting using the HTTP actions - get, post, put and delete.

### What is MVC?

- MVC stands for Model, View, Controller. It is used to define how these three different entities can interact with each other.
- The Controller handles user requests and delegates information between the Model and the View. It only deals with requests, and never handles data or presentation.
- The Model handles data validation, logic, and persistence. It interacts directly with the database to handle the data. The Controller will get all of its data information by asking the Model about the data.
- The View handles presenting the information. It will usually render dynamic HTML pages based on the data the model fetches. The Controller is responsible for passing that data between the Model and View, so that the Model and View never have to interact with each other.

### Requirements

- MongoDB
- Node.js
- REST Client vscode extension

### Set up

- `npm init`
- `npm i express moogoose`
- `npm i --save-dev dotenv nodemon`

### Run

- `npm run devStart`
- `route.rest` => `Send Request`
- Or visit `localhost:3000`
