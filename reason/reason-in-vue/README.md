# Adding ReasonML to a Vue application

Despite the fact that [ReasonML](https://reasonml.github.io/) is a natural fit for React, thanks to [BuckleScript](https://bucklescript.github.io/) it can easily be used in any JavaScript application. And yes &mdash; this includes [Vue.js](https://vuejs.org/)!

If you're working with Vue and like OCaml/ReasonML, or whether you've heard all the hype and are curious to try &mdash; in this article I will show how to use code written in Reason from Vue.

Note: This is the second article in my miniseries about integrating Reason into an existing codebase. For a more basic explanation about how everything hangs together, check out the first article: [Adding ReasonML to an existing codebase](https://github.com/Yakimych/reason-in-typescript/blob/master/basic-javascript/README.md). In [Part 3](https://github.com/Yakimych/articles/tree/master/react-typescript) we're going to integrate Reason into a [React](https://reactjs.org/) [TypeScript](https://www.typescriptlang.org/) codebase.

## Step 0: Starting point

Our starting point is a freshly created Vue application with the help of the [Vue CLI](https://cli.vuejs.org/guide/creating-a-project.html) default preset. It is worth noting that thanks to [genType](https://github.com/cristianoc/genType), this guide would work equally well for a [TypeScript](https://www.typescriptlang.org/) application.

## Step 1: Adding BuckleScript

We are going to need [BuckleScript](https://bucklescript.github.io/) for compiling ReasonML or OCaml code to JavaScript and [genType](https://github.com/cristianoc/genType) in order to simplify interop between Reason and JS. More about this in [Part 1](https://github.com/Yakimych/reason-in-typescript/tree/master/basic-javascript) of the mini series.

Let's go ahead and install the packages:

```
npm install --save-dev bs-platform gentype
npm install -g bs-platform
```

We're going to need to make sure `bucklescript` runs before `babel`, so let's add the command to the `start` and `build` scripts in `package.json`:

```json
"scripts": {
  "serve": "bsb -make-world && vue-cli-service serve",
  "build": "bsb -make-world && vue-cli-service build"
}
```

The last thing left before we can start writing code is to add [bsconfig.json](https://bucklescript.github.io/docs/en/build-configuration.html):

```json
{
  "name": "reason-in-vue",
  "sources": [
    {
      "dir": "src/reason",
      "subdirs": true
    }
  ],
  "package-specs": [
    {
      "module": "es6-global",
      "in-source": true
    }
  ],
  "suffix": ".bs.js",
  "namespace": true,
  "refmt": 3,
  "gentypeconfig": {
    "language": "untyped"
  }
}
```

## Step 2: Writing a function in Reason

Note that `src/reason` is specified as the sources directory, so let's create it and add a `TestFunctions.re` file so that we can test our setup:

```ocaml
let reasonSum = (a, b) => a + b;
```

If you're using [VS Code](https://code.visualstudio.com/) with the [reason-language-server](https://github.com/jaredly/reason-language-server) extension, a `TestFunctions.bs.js` file will immediately get generated next to the `.re` file:

```javascript
function reasonSum(a, b) {
  return (a + b) | 0;
}
```

Annotating the function with `[@genType]` would produce a `TestFunctions.gen.js` file next to `TestFunctions.bs.js`:

```ocaml
[@genType]
let reasonSum = (a, b) => a + b;
```

```javascript
import * as Curry from "bs-platform/lib/es6/curry.js";

import * as TestFunctionsBS from "./TestFunctions.bs";

export const reasonSum = function(Arg1, Arg2) {
  const result = Curry._2(TestFunctionsBS.reasonSum, Arg1, Arg2);
  return result;
};
```

At this point we can use the `reasonSum` function from JavaScript &mdash; let's call it from our Vue component:

```javascript
<template>
  <div id="app">
    <div>The result is {{ result }}</div>
  </div>
</template>

<script>
import { reasonSum } from "./reason/TestFunctions.gen";

export default {
  name: "app",
  data() {
    return {
      result: reasonSum(1, 2)
    };
  }
};
</script>
```

Note that if you're running from the terminal and would like changes in Reason files to get transpiled and picked up on the fly, we would need to have `bsb -make-world -w` running in the background:

![Compilation on the fly](https://user-images.githubusercontent.com/5010901/58440664-e6ce8c80-80db-11e9-8eaf-e3055ba954a9.gif)

## Step 3: Calling the API and decoding the response in Reason

The next step is adding an API call that will fetch some interesting information about a random number from http://numbersapi.com.

A call to `http://numbersapi.com/random/math?json` would produce the following response:

```json
{
  "text": "880 is the number of 4×4 magic squares.",
  "number": 880,
  "found": true,
  "type": "math"
}
```

We're going to make the API call with [bs-axios](https://github.com/meafmira/bs-axios) and decode the response with [bs-json](https://github.com/glennsl/bs-json):

```
npm install --save bs-axios @glennsl/bs-json
```

An important step that is easy to forget is adding those dependencies to `bsconfig.json`:

```json
  "bs-dependencies": ["@glennsl/bs-json", "bs-axios"]
```

Now we can create a new file `NumberFacts.re`, model the type, and create a decoder:

```ocaml
[@genType]
type numberFact = {
  number: int,
  text: string,
  isFound: bool,
};

module Decode = {
  let fact = json =>
    Json.Decode.{
      number: json |> field("number", int),
      text: json |> field("text", string),
      isFound: json |> field("found", bool),
    };
};
```

The API call itself can be performed this way:

```ocaml
[@genType]
let fetchNumberFact = () =>
  Js.Promise.(
    Axios.get("http://numbersapi.com/random/math?json")
    |> then_(response => response##data |> Decode.fact |> resolve)
  );
```

The inferred type in Reason is `unit => Js.Promise.t(numberFact)`, as expected. The generated JavaScript code in `NumberFacts.gen.js` function looks like this:

```typescript
import * as NumberFactsBS from "./NumberFacts.bs";

export const fetchNumberFact = function(Arg1) {
  const result = NumberFactsBS.fetchNumberFact(Arg1);
  return result.then(function _element($promise) {
    return { number: $promise[0], text: $promise[1], isFound: $promise[2] };
  });
};
```

I explain the differences between the code generated by BuckleScript and genType in the [first article](https://github.com/Yakimych/reason-in-typescript/blob/master/basic-javascript/README.md) of this miniseries.

## Step 4: Tying it all together

This is all we have to do on the Reason side of things. Now it is time to call our function from the Vue component and display the result:

```javascript
<template>
  <div id="app">
    <div class="number-fact">
      <div>Number: {{numberFact.number}}</div>
      <div>Fact: "{{numberFact.text}}"</div>
      <div>{{numberFact.isFound ? "Found" : "Not found!"}}</div>
      <button @click="fetchNewFact">Fetch new fact</button>
    </div>
  </div>
</template>

<script>
import { fetchNumberFact } from "./reason/NumberFacts.gen";

export default {
  name: "app",
  data() {
    return {
      numberFact: {
        number: -1,
        text: "-1 is not an interesting number",
        isFound: false
      }
    };
  },
  methods: {
    fetchNewFact: function() {
      fetchNumberFact().then(numberFact => (this.numberFact = numberFact));
    }
  },
  mounted: function() {
    this.fetchNewFact();
  }
};
</script>
```

A new fact will be automatically loaded after the component is mounted. Clicking the "Fetch new fact" button would load a fresh random number fact &mdash; all done via ReasonML code.

## Summary

Adding ReasonML to an existing Vue codebase can be done in a matter of minutes. After this initial setup, it becomes possible to write logic in ReasonML or OCaml and use it in existing Vue components. The source code is available on [GitHub](https://github.com/Yakimych/articles/tree/master/reason-in-vue).

Hopefully this tutorial will inspire Vue.js developers to try ReasonML!
