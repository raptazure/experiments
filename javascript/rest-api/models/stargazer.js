const mongoose = require('mongoose');

const stargazerSchema = new mongoose.Schema({
  name: {
    type: String,
    required: true
  },
  starRepo: {
    type: String,
    required: true
  },
  starDate: {
    type: Date,
    required: true,
    default: Date.now()
  }
});

module.exports = mongoose.model('Stargazer', stargazerSchema);