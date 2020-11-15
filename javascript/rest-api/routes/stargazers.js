const express = require('express');
const router = express.Router();
const Stargazer = require('../models/stargazer');

// Getting All
router.get('/', async (req, res) => {
  try {
    const stargazers = await Stargazer.find();
    res.json(stargazers);
  } catch (err) {
    res.status(500).json({
      message: err.message
    });
  }
});

// Getting One
router.get('/:id', getStargazer, (req, res) => {
  res.json(res.stargazer);
});

// Creating One
router.post('/', async (req, res) => {
  const stargazer = new Stargazer({
    name: req.body.name,
    starRepo: req.body.starRepo,
  });

  try {
    const newStargazer = await stargazer.save();
    res.status(201).json(newStargazer);
  } catch (err) {
    res.status(400).json({
      message: err.message
    });
  }

});

// Updating One
// put would update all the information all at once while
// patch would update just the information that gets passed 
router.patch('/:id', getStargazer, async (req, res) => {
  if (req.body.name != null) {
    res.stargazer.name = req.body.name;
  }
  if (req.body.starRepo != null) {
    res.stargazer.starRepo = req.body.starRepo;
  }
  try {
    const updatedStargazer = await res.stargazer.save();
    res.json(updatedStargazer);
  } catch (err) {
    res.status(400).json({
      message: err.message
    });
  }
});

// Deleting One
router.delete('/:id', getStargazer, async (req, res) => {
  try {
    await res.stargazer.remove();
    res.json({
      message: 'Deleted Stargazer'
    });
  } catch {
    res.status(500).json({
      message: err.message
    });
  }
});

async function getStargazer(req, res, next) {
  let stargazer;
  try {
    stargazer = await Stargazer.findById(req.params.id);
    if (stargazer === null) {
      return res.status(404).json({
        message: 'Cannot find stargazer'
      });
    }
  } catch (err) {
    return res.status(500).json({
      message: err.message
    });
  }

  res.stargazer = stargazer;
  next();
}


module.exports = router;