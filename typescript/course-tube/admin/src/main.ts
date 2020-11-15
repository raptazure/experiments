import Vue from 'vue';
import App from './App.vue';
import axios from 'axios';
import './plugins/element.ts';
import './plugins/avue.js';
import router from './router';

Vue.config.productionTip = false;

// import EleForm from "vue-ele-form";
// Vue.use(EleForm);
const http = axios.create({
  baseURL: process.env.VUE_APP_API_URL,
});

Vue.prototype.$httpajax = http;
Vue.prototype.$http = http;

new Vue({
  router,
  render: (h) => h(App),
}).$mount('#app');
