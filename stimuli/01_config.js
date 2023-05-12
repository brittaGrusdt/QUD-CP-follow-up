const DEBUG = true;
const SHOW_PIC_LABELS = false;
var MODE = "train"
//var MODE = "test"

const SCENE = {w: 800, h: 500};
PROPS = {'blocks': {'w':40, 'h': 80},
         'walls': {'w': 200, 'h': 20},
         'balls': {'radius': 20, 'move_to_roll': 4},
         'bottom': {'w': SCENE.w, 'h': 20},
         'seesaw': {'d_to_walls': 5,
                    'stick': {'w': 20, 'h': 95},
                    'plank': {'w': 220, 'h': 10},
                    'link': {'w': 5, 'h': 10},
                    'base2blocks': {'w': 90, 'h': 20}
                  },
         'if1_ssw': {'stick': {'w': 20, 'h': 25}, 'plank': {'w': 250, 'h': 10}},
         'if1_base_ssw': {w: 80, h: 12}
       };

OPTS = {
  'walls': {isStatic: true, render: {fillStyle: COLS.grey}},
  'balls': {isStatic: false, restitution: 0.02, friction: 0.0001, density: 0.002},
  'blocks': {isStatic: false, density: 0.001, restitution: .02, friction: 0.001},
  'plank': {isStatic: false, density: 0.001, restitution: .02, friction: 0.1}
}

let TEST_TYPES = ['if1', 'if2ssw', 'independent', 'independent_edge', 'if2',
                  'independent_edge3', 'if1_ind'];

// Proportion of block that's ON TOP of its base wall beneath
let PRIOR = {
  'vertical': {'high': 0.40, 'uncertainH': 0.52, 'uncertain': 0.54,
               'uncertainL': 0.55, 'lowH': 0.62, 'low': 0.70, 'never': 1.25
              },
  'horizontal': {'high': 0.42, 'uncertainH': 0.5, 'uncertain': 0.51,
                 'uncertainL': 0.53, 'lowH': 0.6, 'low': 0.67, 'lowL': 0.70,
                 'never': 1.25
                },
  'impossible': 1.25,
  'conditions': ['high', 'uncertain', 'low', 'never',
                 'uncertainH', 'uncertainL']
}

// shift of ramp walls such that there is no edge (should not be changed)
let OVERLAP_SHIFT = {
  "angle45": 16,
  "angle42": 15, "angle40": 15, "angle38": 19, "angle35": 16,
  "angle32": 12, "angle30": 12,
  "angle29": 12, "angle28": 13, "angle27": 12, "angle26": 12, "angle25": 12,
  "angle23": 10, "angle20": 9, "angle18": 9
}

// one of the angles specified in OVERLAP_SHIFT
let ANGLES = {
  'default': 32
}

// used for pre-experiment trying out different angles for the ramp
let PRETEST_ANGLES = _.range(45);

// base (lower) wall of ramp, its width depends on the prior/direction of block
// that will be put on top. longer widths for lower probabilities of block to fall
// values are specific for default angle (of 32), if different angle is used,
// these values will probabily need to be respecified
let BASE_RAMP = {
  'horizontal': {'high': 125, 'uncertainH': 130, 'uncertain': 165,
                 'uncertainL': 200, 'low': 270, 'xlow': 300, 'never': 270},
  'vertical': {'high': 130, 'uncertainH': 220, 'uncertain': 255,
               'uncertainL': 290, 'low': 320, 'xlow': 400, 'never': 320},
  'default': 200
};

let SIMULATION = {'duration': 5000};

// how long is the base wall next to ramp,
// high -> low probability for block to fall;
// .. low -> high probability for block to fall;
let IF1_BASE_RAMP = "high"
//let IF1_IS_EXHAUSTIVE = true
//let TRAIN_TYPES = ['ramp', 'uncertain', 'if2ssw', 'independent', 'if1', 'cp']
let TRAIN_IDS = {
  'cp': ['independent_edge_nh',
         'if1_hn',
         'if1_nn',
         'if1_ln',
         'if2_nnh',
         'if2_hnh',
         'if2_nnl',
         // additional train trial that was not part of Exp. 1
         //(scene that contradicts conditional `if ant-block, cons-block`)
         'if1_ind_hn'
       ]
  }

let train_expectations = {
  'independent_edge_nh': ['CONS'],
  'if1_hn': ['ANT', 'CONS'],
  'if1_nn': ['none'],
  'if1_ln': ['none'],
  'if2_nnh': ['CONS', 'yellow'],
  'if2_hnh': ['ANT', 'CONS', 'yellow'],
  'if2_nnl': ['none'],
  'if1_ind_hn': ['ANT'],

  // other train trials that we did not use in the Experiment
  'independent_edge_nn': ['none'],
  'if1_u-Ln': ['none'],
  'if1_u-Hh': ['ANT', 'CONS'],
  'if2_u-Lnu-H': ['yellow', 'CONS'],
  'if2_u-Hnu-H': ['yellow', 'ANT', 'CONS'],
  'if2_u-Hnu-L': ['ANT', 'CONS'],
  'if2_u-Lnu-L': ['none'],
  'if2_nnl': ['none']
}
// SPECIFY TRAIN ORDER: alternate between trials where sth happens and trials
// where nothing happens
train_expectations_ids = _.filter(Object.keys(train_expectations), function(id){
  return(TRAIN_IDS.cp.includes(id))
});
let train_expect_none = []
let train_expect_sth = []
_.forEach(train_expectations_ids, function(id, i){
  train_expectations[id][0] === "none" ? train_expect_none.push(id) : train_expect_sth.push(id);
});

// order st. sth happens alternates with nothing happens and if1_hn followed by if1_ln
let trials_fixed_order = ["if1_hn", "if1_ln"]
train_expect_sth = _.filter(train_expect_sth, function(id){
    return(trials_fixed_order.indexOf(id) == -1)
});
train_expect_none = _.filter(train_expect_none, function(id){
  return(trials_fixed_order.indexOf(id) == -1)
})
// simple alternating between two types
TRAIN_ORDER = _.filter(_.flatten(_.zip(_.shuffle(train_expect_sth),
                                       _.shuffle(train_expect_none))),
                           function(id){ return id !== undefined});
// add two trials that shall appear in fixed order:
TRAIN_ORDER = trials_fixed_order.concat(TRAIN_ORDER);

COLS_GROUPS = {group1: {ANT: "BLUE", CONS: "GREEN"},
               group2: {ANT: "GREEN", CONS: "BLUE"},
               example: {}
              }
QUESTS = {
  neutral: '<b>Which blocks do you think will fall?</b>',
  willq: '<b>Will the CONS block fall?</b>',
  ifp: '<b>What happens if the ANT block falls?</b>',
}

ANSWERS = {
  if_pq: 'If the ANT block falls, the CONS block will fall.',
  if_p_or_yellow_q: 'If the ANT or the YELLOW block falls, the CONS block will fall.',
  even_if_p_not_q: 'Even if the ANT block falls, the CONS block will NOT fall.',
  not_q: 'The CONS block will NOT fall.',
  q: 'The CONS block will fall.',
  p_and_q: 'The CONS and the ANT block will fall.',
  neither_p_nor_q: 'NEITHER the ANT NOR the CONS block will fall.',
  might_p: 'The ANT block MIGHT fall.',
  yellow: 'The YELLOW block will fall.',
  yellow_and_q: 'The YELLOW and the CONS block will fall.',
  all3: 'The YELLOW, the ANT and the CONS block will fall.',
  neither_yellow_nor_q: 'NEITHER the YELLOW NOR the CONS block will fall.',
  blue_green: 'The BLUE and the GREEN block will fall.'
}

TEXT_BOBS_SCREENS = {
  attention_check: '<small>Please click on the SIDE picture.</small>',
  normal_check: '<small>Please select the picture(s) that you think Ann and Bob are talking about.</small>'
}
PATH_BOBS_SCREEN = "stimuli/img/bobs-screen.jpeg"
PATH_PICTURES = "stimuli/img/group/"

TEST_DATA = [
  //example to take screenshot for instructions
  //{id: "trial_ex", pic1: "ind_edge_hn", pic2: "ind_edge_hh_v2", pic3: "ind_edge_hh", pic0: "ant_ind_h", property_pic1: "control", property_pic2: "exhaustive", property_pic3: "non-exhaustive", question: QUESTS.neutral, answer: ANSWERS.p_and_q, type: 'example-instruction'},

  // critical trials (pic1: control, pic2: exhaustive, pic3: non-exhaustive)
  {id: "trial1", pic1: "if1_un_ind", pic2: "if1_un", pic3: "if2_unu", pic0: "ant_u", property_pic1: "contrast", property_pic2: "exhaustive", property_pic3: "non-exhaustive", question: QUESTS.ifp, answer: ANSWERS.if_pq, type: 'critical', expected:'', bob: _.sample(["pic2", "pic3"])},
  {id: "trial2", pic1: "if1_un_ind", pic2: "if1_un", pic3: "if2_unu", pic0: "cons_n", property_pic1: "contrast", property_pic2: "exhaustive", property_pic3: "non-exhaustive", question: QUESTS.willq, answer: ANSWERS.if_pq, type: 'critical', expected:'', bob: _.sample(["pic2", "pic3"])},
  {id: "trial3", pic1: "if1_un_ind", pic2: "if2_unn_horiz", pic3: "if1_uu", pic0: "ant_u", property_pic1: "contrast", property_pic2: "exhaustive", property_pic3: "non-exhaustive", question: QUESTS.ifp, answer: ANSWERS.if_pq, type: 'critical', expected:'', bob: _.sample(["pic2", "pic3"])},

  {id: "trial4", pic1: "if1_un_ind", pic2: "if1_un", pic3: "if1_uu", pic0: "ant_u", property_pic1: "contrast", property_pic2: "exhaustive", property_pic3: "non-exhaustive", question: QUESTS.ifp, answer: ANSWERS.if_pq, type: 'critical', expected:'', bob: _.sample(["pic2", "pic3"])},
  {id: "trial5", pic1: "if1_un_ind", pic2: "if2_unn_horiz", pic3: "if2_unu", pic0: "ant_u", property_pic1: "contrast", property_pic2: "exhaustive", property_pic3: "non-exhaustive", question: QUESTS.ifp, answer: ANSWERS.if_pq, type: 'critical', expected:'', bob: _.sample(["pic2", "pic3"])},
  {id: "trial6", pic1: "if1_un_ind", pic2: "if2_unn_horiz", pic3: "if2_unu", pic0: "cons_n", property_pic1: "contrast", property_pic2: "exhaustive", property_pic3: "non-exhaustive", question: QUESTS.willq, answer: ANSWERS.if_pq, type: 'critical', expected:'', bob:  _.sample(["pic2", "pic3"])},

  // practice trials
  {id: "trial7", pic1: "if1_un", pic2: "if2_unn_horiz", pic3: "if2_unu", pic0: "cons_n", property_pic1: "contrast", property_pic2: "literal", property_pic3: "pragmatic", question: QUESTS.neutral, answer: ANSWERS.if_p_or_yellow_q, type: 'practice', expected: 'pic3', bob:'pic3'},
  {id: "trial8", pic1: "if2_nnn_horiz", pic2: "if2_nnh_right", pic3: "if2_nnh", pic0: "ant_n", property_pic1: "contrast", property_pic2: "pragmatic", property_pic3: "literal", question: QUESTS.neutral, answer: ANSWERS.yellow, type: 'practice', expected:'', bob:'pic2'},
  {id: "trial11", pic1: "if1_un_ind", pic2: "if1_un", pic3: "if1_un_up", pic0: "cons_n", property_pic1: "contrast", property_pic2: "literal", property_pic3: "literal", question: QUESTS.willq, answer: ANSWERS.if_pq, type: 'practice', expected:'pic2_pic3', bob: _.sample(["pic2", "pic3"])},
  {id: "trial25", pic1: "if2_unn_horiz", pic2: "if1_uu", pic3: "if1_un_ind", pic0: "ant_u", property_pic1: "contrast", property_pic2: "contrast", property_pic3: "literal", question: QUESTS.ifp, answer: ANSWERS.even_if_p_not_q, type: 'practice', expected:'pic3', bob:'pic3'},

  // filler-trials
  {id: "trial10", pic1: "if1_un_ind", pic2: "if1_un", pic3: "if2_unh_ind", pic0: "ant_u", property_pic1: "contrast", property_pic2: "literal", property_pic3: "contrast", question: QUESTS.ifp, answer: ANSWERS.if_pq, type: 'filler', expected:'pic2', bob: 'pic2'},
  {id: "trial13", pic1: "if2_unn_ind_horiz", pic2: "if2_unn_horiz", pic3: "if2_unn_up_horiz", pic0: "cons_n", property_pic1: "contrast", property_pic2: "literal", property_pic3: "literal", question: QUESTS.willq, answer: ANSWERS.if_pq, type: 'filler', expected:'pic2_pic3', bob: _.sample(["pic2", "pic3"])},
  {id: "trial19", pic1: "ind_edge_hh", pic2: "ind_edge_hnh", pic3: "ind_edge_hnn_horiz", pic0: "ant_ind_h", property_pic1: "contrast", property_pic2: "pragmatic", property_pic3: "literal", question: QUESTS.neutral, answer: ANSWERS.not_q, type: 'filler', expected:'', bob: _.sample(["pic2", "pic3"])},
  {id: "trial20", pic1: "ind_edge_nn", pic2: "ind_edge_nhn", pic3: "ind_edge_nhh", pic0: "ant_ind_n", property_pic1: "contrast", property_pic2: "pragmatic", property_pic3: "literal", question: QUESTS.neutral, answer: ANSWERS.q, type: 'filler', expected:'', bob: _.sample(["pic2", "pic3"])},
  {id: "trial21", pic1: "if1_nh", pic2: "if2_nnh_right_ind", pic3: "if2_nnn_horiz", pic0: "ant_n", property_pic1: "contrast", property_pic2: "pragmatic", property_pic3: "literal", question: QUESTS.neutral, answer: ANSWERS.neither_p_nor_q, type: 'filler', expected:'', bob: _.sample(["pic2", "pic3"])},
  {id: "trial24", pic1: "if1_nn_ind", pic2: "if2_nnh", pic3: "if2_hnh_right", pic0: "cons_n", property_pic1: "contrast", property_pic2: "pragmatic", property_pic3: "literal", question: QUESTS.willq, answer: ANSWERS.yellow_and_q, type: 'filler', expected:'pic2', bob:'pic2'},

  // attention check trial
  {id: "trial23", pic1: "ind_edge_hh", pic2: "ind_edge_hnh", pic3: "ind_edge_hnn_horiz", pic0: "ant_ind_h", property_pic1: "contrast", property_pic2: "contrast", property_pic3: "literal", question: QUESTS.neutral, answer: ANSWERS.neither_yellow_nor_q, type: 'attention-check', expected:'pic3', bob:'pic3'}
];

PRACTICE_IDS = _.map(_.filter(TEST_DATA, function(obj){
  return obj.type == 'practice'
}), 'id')

TEST_IDS = _.map(TEST_DATA, 'id')

// feedback messages
const FEEDBACK = {
  scores: {fail: -100, all: -100, success: 100, win: 50},
  msg: {
    fail: 'You LOOSE 100 points!',
    all: 'Ups - you selected all 3 scenes... So, you LOOSE 100 points! ',
    success: 'Awesome - your choice was correct! You GET 100 points!',
    win: 'So you GET 50 points!'
  }
}


// position of antecedent-block in if2ssw/independent-trials wrt its base platform
// (here probability to fall depends on width of ramp as ball is moving on its own)
let IF2_POS_BLOCK_RAMP = "never"
let IND_POS_BLOCK_RAMP = "never"
// distance to ramp is made larger when prior of a horizontal block (assured in
// makeTestStimuli) in if2ssw/independent-trials, where uncertainty comes from
// width of ramp base, is low
let DIST_POS_BLOCK_RAMP_LOW = 50

// fine-grained uncertain priors, e.g. u-Ll/u-Hl do not need extra entry, they
// shall be treated as their corresponding trial with a simple uncertain prior.
let HORIZ_AC2 = {
  'nn': ['vertical', 'horizontal'], 'nl': ['vertical', 'horizontal'],
  'nu': ['vertical', 'horizontal'], 'nh': ['vertical', 'horizontal'],

  'ln': ['vertical', 'vertical'], 'll': ['horizontal', 'horizontal'],
  'lu': ['vertical', 'horizontal'], 'lh': ['horizontal', 'vertical'],

  'un': ['vertical', 'vertical'], 'ul': ['vertical', 'horizontal'],
  'uu': ['horizontal', 'horizontal'], 'uh': ['vertical', 'horizontal'],

  'hn': ['vertical', 'vertical'], 'hl': ['vertical', 'horizontal'],
  'hu': ['vertical', 'horizontal'], 'hh': ['vertical', 'vertical'],
}

let HORIZ_IND = {
  'nn': ['vertical', 'vertical'], 'nl': ['vertical', 'vertical'],
  'nu': ['vertical', 'vertical'], 'nh': ['vertical', 'vertical'],

  'ln': ['vertical', 'horizontal'], 'll': ['vertical', 'horizontal'],
  'lu': ['vertical', 'horizontal'], 'lh': ['horizontal', 'vertical'],
  'un': ['vertical', 'horizontal'], 'ul': ['vertical', 'horizontal'],
  'uu': ['horizontal', 'vertical'], 'uh': ['horizontal', 'vertical'],
  'hn': ['vertical', 'horizontal'], 'hl': ['horizontal', 'horizontal'],
  'hu': ['horizontal', 'vertical'], 'hh': ['vertical', 'vertical']
}

let HORIZ_AC1 = {
  'nn': ['vertical', 'vertical'], 'nl': ['vertical', 'vertical'],
  'nu': ['vertical', 'vertical'], 'nh': ['vertical', 'vertical'],

  'ln': ['vertical', 'vertical'], 'll': ['vertical', 'vertical'],
  'lu': ['vertical', 'vertical'], 'lh': ['vertical', 'vertical'],

  'un': ['vertical', 'vertical'], 'ul': ['vertical', 'vertical'],
  'uu': ['vertical', 'vertical'], 'uh': ['vertical', 'vertical'],

  'hn': ['vertical', 'vertical'], 'hl': ['vertical', 'vertical'],
  'hu': ['vertical', 'vertical'], 'hh': ['vertical', 'vertical']
}
