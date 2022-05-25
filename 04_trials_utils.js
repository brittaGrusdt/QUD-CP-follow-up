// ANIMATION TRAINING TRIALS
// training trials given in TRAIN_IDS in random order
pseudoRandomTrainTrials = function(){
  let stimuli = []
  // get data for building up stimuli
  let dict = Object.keys(TRAIN_IDS).forEach(function(key){
    let trials = TrainStimuli.map_category[key]
    let trial_ids = _.map(Object.values(trials), 'id')
    // only use trials specified in 01_config
    let vals = _.filter(Object.values(trials), function(obj){
      return(TRAIN_IDS[key].includes(obj.id))
    })
    stimuli = stimuli.concat(vals)
  });
  let trials = _.map(_.range(0, stimuli.length), function(i){
      return(getTrialById(TRAIN_TRIALS, stimuli[i].id))
  })

  let indices = [];
  let ids = _.map(stimuli, 'id')
  _.map(TRAIN_ORDER, function(id){
    indices.push(ids.indexOf(id))
  });
  let stimuli_data = indices.map(i => stimuli[i]);
  let trial_data = indices.map(i => trials[i]);

  return {stimuli_data, trial_data}
}

let training = pseudoRandomTrainTrials()
const SHUFFLED_TRAIN_STIMULI = training.stimuli_data;
const SHUFFLED_TRAIN_TRIALS = training.trial_data;

// TEST TRIALS
randomTestIds= function(){
  return _.shuffle(_.map(TEST_DATA, 'id'))
}

pseudoRandomTestIds = function(){
  let critical_ids = _.shuffle(_.map(_.filter(TEST_DATA, function(dat){
    return dat.type === "critical"
  }), 'id'));

  let practice_ids = _.shuffle(_.map(_.filter(TEST_DATA, function(dat){
    return dat.type === "practice"
  }), 'id'));

  let filler_ids = _.shuffle(_.map(_.filter(TEST_DATA, function(dat){
    return dat.type === "filler"
  }), 'id'));

  // first attention check is for practice block always the same
  let attention_check_ids = _.shuffle(_.map(_.filter(TEST_DATA, function(dat){
    return dat.type === "attention-check"
  }), 'id'));
  // testing trials
  let test_ids = _.flatten(_.zip(filler_ids, critical_ids));
  // just one attention-check trial in middle of test phase
  test_ids.splice(test_ids.length/2, 0, attention_check_ids[0]);
  let practice_and_test_ids = practice_ids.concat(test_ids)
  return(practice_and_test_ids)
}


// save trial data in specified pseudorandom order s.t. accessible in experiment
let shuffleTestTrials = function(trial_data){
  let shuffled_trials = [];
  let trial_ids = _.map(trial_data, 'id'); // data for all to be used test-ids
  const ids_sequence = pseudoRandomTestIds();
  //const ids_sequence = randomTestIds();
  ids_sequence.forEach(function(id){
    let idx = _.indexOf(trial_ids, id)
    if(idx === -1) console.warn('Test trial with id: ' + id +  ' not found.')
    shuffled_trials.push(trial_data[idx]);
  });
  return shuffled_trials;
}

const TEST_TRIALS = shuffleTestTrials(image_selection_trials);
const PRACTICE_TRIALS = _.filter(TEST_TRIALS, function(obj){
  return(obj.type === "practice")
});

_.map(TEST_TRIALS, function(obj, idx){
  obj.block = idx <= 3 ? "practice" :
              idx <= 7 ? "test1" :
              idx <= 12 ? "test2" : "test3";
  return(obj)
})
const TEST_TRIALS_01 = TEST_TRIALS.slice(4, 11);
const TEST_TRIALS_02 = TEST_TRIALS.slice(11);
