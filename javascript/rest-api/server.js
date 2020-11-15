require('dotenv').config();

const express = require('express');
const mongoose = require('mongoose');
const app = express();

mongoose.connect(process.env.DATABASE_URL, {
  useUnifiedTopology: true,
  useNewUrlParser: true
});

const db = mongoose.connection;
db.on('error', (error) => console.log(error));
db.once('open', () => console.log('Connected to Database'));

app.use(express.json());

const stargazersRouter = require('./routes/stargazers');
app.use('/stargazers', stargazersRouter);

app.listen(3000, () => console.log('Server Started'));