const fetch = require('node-fetch');
const countriesAPI = 'https://restcountries.eu/rest/v2/all';
const catsAPI = 'https://api.thecatapi.com/v1/breeds';

const fetchData = async (url) => {
  try {
    const response = await fetch(url);
    const data = await response.json();
    return data;
  } catch (err) {
    console.log(err);
  }
};

const findWeight = async () => {
  try {
    const cats = await fetchData(catsAPI);
    const res = {};
    for (const cat of cats) {
      const catName = cat.name;
      const weightStr = cat.weight.metric.split('-');
      const averWeight = (parseInt(weightStr[0]) + parseInt(weightStr[1])) / 2;
      res[catName] = averWeight;
    }
    console.log(res);
  } catch (err) {
    console.log(err);
  }
};

const largeCountries = async () => {
  try {
    const countries = await fetchData(countriesAPI);
    const res = {};
    for (const country of countries) {
      const name = country.name;
      res[name] = country.area;
    }
    const list = Object.keys(res).map((key) => [key, res[key]]);
    console.log(list.sort((a, b) => b[1] - a[1]).slice(0, 10));
  } catch (err) {
    console.log(err);
  }
};

const languageSet = async function () {
  try {
    const countries = await fetchData(countriesAPI);
    const res = new Set();
    for (const country of countries) {
      const languageList = country.languages;
      for (const language of languageList) {
        res.add(language.name);
      }
    }
    console.log(res.size);
  } catch (err) {
    console.log(err);
  }
};

findWeight();
largeCountries();
languageSet();