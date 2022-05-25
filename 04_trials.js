// In this file you can specify the trial data for your experiment

// Test trials
// put data together defined in stimili/01_config.js
var image_selection_trials = [];
TEST_DATA.forEach(function(dat) {
  let idx_quest = Object.values(QUESTS).indexOf(dat.question)
    image_selection_trials.push({
      id: dat.id,
      QUD: Object.keys(QUESTS)[idx_quest],
      // question_long: "<b>Ann</b>: " + dat.question,
      // answer: "<b>Bob</b>: " + dat.answer,
      question_long: dat.question,
      answer: dat.answer,
      pic1: dat.pic1 + ".png",
      pic2: dat.pic2 + ".png",
      pic3: dat.pic3 + ".png",
      pic0: dat.pic0 + ".jpeg",
      type: dat.type,
      property_pic1: dat.property_pic1,
      property_pic2: dat.property_pic2,
      property_pic3: dat.property_pic3,
      bob: dat.bob,
      score: 0,
      expected: dat.expected
    });
});

// add color group
_.map(image_selection_trials, function (trial) {
  let group = _.sample(["group1", "group2"]);
  trial.group = group
});

// ----- TRAINING TRIALS (Buttons) for exp1 + exp2 ---- //
// the data of the training stimuli is always the same, only animation ids differ
let TRAIN_TRIALS = [];
let train_keys = Object.keys(TRAIN_IDS);
train_keys.forEach(function (kind) {
  TRAIN_IDS[kind].forEach(function(id){
    let data = {
      QUD: "Use the buttons below to select all the blocks that you think will fall.<br/>(Multiple selections possible!)",
      id: id,
      question: '',
      expected: train_expectations[id],
      type: 'training'
    };
    TRAIN_TRIALS.push(data);
  });
});
