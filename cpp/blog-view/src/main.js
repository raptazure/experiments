import Vue from "vue";
import axios from "axios";
import App from "./App.vue";
import "./plugins/element.js";
import router from "./router";

Vue.config.productionTip = false;

Vue.prototype.$http = axios.create({
  baseURL: "http://localhost:8000/api",
});

new Vue({
  router,
  render: (h) => h(App),
}).$mount("#app");
