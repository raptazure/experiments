const apiURL = "https://api.github.com/repos/vuejs/vue/commits?per_page=3&sha=";

const demo = new Vue({
  el: '#demo',

  data: {
    branches: ["master", "dev"],
    currentBranch: "master",
    commits: null
  },

  created: function () {
    this.fetchData();
  },

  watch: {
    currentBranch: "fetchData"
  },

  filters: {
    truncate: function (v) {
      const newline = v.indexOf('\n');
      return newline > 0 ? v.slice(0, newline) : v;
    },
    formatDate: function (v) {
      return v.replace(/T|Z/g, ' ');
    }
  },

  methods: {
    fetchData: function () {
      const xhr = new XMLHttpRequest();
      const self = this;
      xhr.open("GET", apiURL + self.currentBranch);
      xhr.onload = function () {
        self.commits = JSON.parse(xhr.responseText);
      };
      xhr.send();
    }
  }
});