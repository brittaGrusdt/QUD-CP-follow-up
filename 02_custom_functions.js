// Here, you can define all custom functions, you want to use and initialize some variables

// You can determine global (random) parameters here

// Declare your variables here

/* For generating random participant IDs */
// https://stackoverflow.com/questions/1349404/generate-random-string-characters-in-javascript
// dec2hex :: Integer -> String
const dec2hex = function (dec) {
  return ("0" + dec.toString(16))
    .substr(-2);
};
// generateId :: Integer -> String
const generateID = function (len) {
  let arr = new Uint8Array((len || 40) / 2);
  window.crypto.getRandomValues(arr);
  return Array.from(arr, this.dec2hex)
    .join("");
};

// Error feedback if participants exceeds the time for responding
const time_limit = function (data, next) {
  if (typeof window.timeout === "undefined") {
    window.timeout = [];
  }
  // Add timeouts to the timeoutarray
  // Reminds the participant to respond after 5 seconds
  window.timeout.push(
    setTimeout(function () {
      $("#reminder")
        .text("Please answer more quickly!");
    }, 5000)
  );
  next();
};

let TOTAL_POINTS = 0;
let POINTS_TEST_PHASE = 0;
let POINTS_PRACTICE = 0;
// compares the chosen answer to the value of `option1`
// check_response = function (data, next) {
//   $("input[name=answer]")
//     .on("change", function (e) {
//       if (e.target.value === data.correct) {
//         alert("Your answer is correct! Yey!");
//       } else {
//         alert(
//           "Sorry, this answer is incorrect :( The correct answer was " +
//           data.correct
//         );
//       }
//       next();
//     });
// };
// custom parameters:
const DURATION_ANIMATION = 7000; // in ms
const KEY2SELECTANSWER = "y";

const NB_TRAIN_TRIALS  = _.flatten(Object.values(TRAIN_IDS)).length
// for training with one button for each of the 4 events:
// const TRAIN_BTTN_IDS = [BLOCK_COLS_SHORT.train.join('')].concat(
//   BLOCK_COLS_SHORT.train).concat(['none']);
// for training with yes-no-undecided buttons:
const TRAIN_BTTN_IDS = ['buttonGreen', 'buttonBlue', 'buttonYellow', 'buttonNone']

//globally initialize
const EVENT_MAP = {"rny": "the red block falls, but not the yellow",
                   "ynr": "the yellow block falls, but not the red",
                   "none": "none of the two blocks falls",
                   "both": "both blocks fall"}

// custom functions:
htmlButtonResponses = function () {
  return `<bttns id="TrainButtons" class="buttonContainer">
    <div class="divider"/>
    <button id="buttonBlue" class="styled-button">
        <img src="stimuli/img/icons/blue.png" alt="blueBlock"/>
    </button>
    <div class="divider"/>
    <button id="buttonGreen" class="styled-button">
      <img src="stimuli/img/icons/green.png" alt="greenBlock"/>
    </button>
    <div class="divider"/>
    <button id="buttonYellow" class="styled-button">
      <img src="stimuli/img/icons/yellow.png" alt="yellowBlock"/>
    </button>
    <div class="divider"/>
    <button id="buttonNone" class="styled-button">None</button>
  </bttns>
  <p id=trainBttnSelect align="center" style="font-size:14px;"><b>&emsp;Please make a selection!</b></p>
  `;
}

htmlRunNextButtons = function () {
  let htmlBttns =
    `<button id='runButton' class='grid-button magpie-view-button'>RUN</button>
     <button id='buttonNextAnimation' class='grid-button magpie-view-button' style='visibility:hidden'>NEXT ANIMATION</button>`;
  return htmlBttns;
}

toggleNextIfDone = function (button, condition) {
  if (condition) {
    button.removeClass("grid-button");
  }
};

getButtonResponse = function() {
  let responses = []
  let trial_data = {}
  TRAIN_BTTN_IDS.forEach(function(id, i){
    let selected = $('#' + id).hasClass('selected');
    if(selected) {
      let block_color = id.replace("button", "").toLowerCase();
      responses.push(block_color)
    }
  });
  return Object.assign(trial_data, {
    'response': responses.sort()
  });
}

showAnimationInTrial = function (CT, html_answers, progress_bar = true) {
  let html_bar = progress_bar ? `<div class='progress-bar-container'>
       <div class='progress-bar'></div>
      </div>` : ``;
  const view_template = html_bar +
    `<div>
      <animationTitle class='stimulus'>
        <h2>${TRAIN_TRIALS[CT].QUD}</h2>
      </animationTitle>
      <animation id='animationDiv'></animation>
      </div>` + html_answers

  $('#main')
    .html(view_template);

  let stimulus = SHUFFLED_TRAIN_STIMULI[CT];
  if (DEBUG) {
    console.log(stimulus.id)
  }

  let worldElems = createWorld();
  let engine = worldElems.engine;
  let render = worldElems.render;
  let add_bottom = stimulus.id === "uncertain_2" ? false : true;
  stimulus.objs = stimulus.objs.concat(movingIrrelevantObj());
  addObjs2World(stimulus.objs, engine, add_bottom);
  show(engine, render);
  let startTime = Date.now();

  return {
    engine,
    render,
    startTime
  }
}

getTrialById = function(trials_all, id){
  let trial = _.filter(trials_all, function(trial){
    return trial.id == id
  })
  trial.length != 1 ? console.error('several trials with id ' + id) : null;
  return trial[0];
}

/*@arg answers: "sliders", "buttons"*/
functionalityRunBttn = function(anim, answers){
  let animation = anim.animation;
  let CT = anim.CT;
  let id = SHUFFLED_TRAIN_STIMULI[CT].id
  let ids_correct = train_expectations[id];
  let ant_cons_colors = map_ant_cons_to_colors(id, first_upper_case=true)
  _.forEach(ids_correct, function(obj_correct, i) {
    ids_correct[i] =
      obj_correct == "ANT" ? "button" + obj_correct.replace("ANT", ant_cons_colors[0]) :
      obj_correct == "CONS" ? "button" + obj_correct.replace("CONS", ant_cons_colors[1]) :
      obj_correct == "yellow" ? "buttonYellow" :
      obj_correct == "none" ? "buttonNone" : obj_correct;
  });
  $('#runButton')
    .on('click', function (e) {
      if (!anim.started) {
        anim.started = true;
        runAnimation(animation.engine);
        // $('#runButton').addClass("grid-button");
        this.remove();
        $('#buttonNextAnimation').css("visibility", "visible");
        _.forEach(ids_correct, function(id){
          $('#' + id).addClass('correct')
        });
        $('#trainBttnSelect').html(`<b>&emsp;Blocks that actually fell are marked in green (light if selected, dark if not selected),
          <br/>&emsp;blocks that were selected but did not fall
          are marked in red.</b>`);

        _.map(TRAIN_BTTN_IDS, function(id){
          $('#' + id).addClass('train-not-clickable');
          if($('#' + id).hasClass('selected') && !ids_correct.includes(id)) {
            $('#' + id).addClass('incorrect')
          }
          if(!$('#' + id).hasClass('selected') && ids_correct.includes(id)) {
            _.forEach(_.range(1,4), function(i){
              $("#" + id).fadeOut(200).fadeIn(200);
              $('#' + id).addClass('forgotten');
            })
          }

        });
        $('#comment').append(SHUFFLED_TRAIN_TRIALS[CT].comment)
        toggleNextIfDone($("#buttonNextAnimation"), true);
      }
    });
}

map_ant_cons_to_colors = function(current_id, first_upper_case=false){
  let ant = _.filter(TrainStimuli.map_category.cp[current_id].objs, function(obj){
    return(obj.label === 'blockA')
  })[0];
  let hex_ant = ant.render.fillStyle;
  let colors_ant_cons = COLS_OBJS_HEX.test_blocks[0] == hex_ant ?
    [BLOCK_COLS.test[0], BLOCK_COLS.test[1]] :
    [BLOCK_COLS.test[1], BLOCK_COLS.test[0]];
  if(first_upper_case) {
    _.forEach(colors_ant_cons, function(col, i){
      colors_ant_cons[i] = col[0].toUpperCase() + col.slice(1);
    })
  }
  return(colors_ant_cons)
}

functionalityBttnNextAnimation = function(response_fn, magpie, anim){
  $("#buttonNextAnimation")
    .on("click", function () {
      let animation = anim.animation
      const RT = Date.now() - animation.startTime; // measure RT before anything else
      let CT = anim.CT
      if (!anim.cleared) {
        clearWorld(animation.engine, animation.render, stop2Render = false);
      }
      let data = response_fn();
      let current_id = SHUFFLED_TRAIN_TRIALS[CT].id

      // map expected blocks ANT and CONS to randomly assigned colors
      let colors_ant_cons = map_ant_cons_to_colors(current_id);

      let expected = SHUFFLED_TRAIN_TRIALS[CT].expected;
      _.range(expected.length).forEach(function(idx) {
        let val = expected[idx].replace("button", "").toLowerCase();
        val = val.replace("ANT", colors_ant_cons[0]);
        val = val.replace("CONS", colors_ant_cons[1]);
        expected[idx] = val
      });

      let trial_data = Object.assign(data, {
        trial_name: anim.trial_name,
        trial_number: CT + 1,
        RT: RT,
        id: current_id,
        expected: expected.sort()
      });
      // copied.expected = TrainExpectations[trial_data.id];
      trial_data = magpieUtils.view.save_config_trial_data(
        SHUFFLED_TRAIN_TRIALS[CT],
        trial_data
      );
      magpie.trial_data.push(trial_data);
      magpie.findNextView();
    });
}
