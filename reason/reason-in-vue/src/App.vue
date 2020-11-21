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

<style>
#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}
</style>
