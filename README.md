# QUD-CP-follow-up
This repository contains all code and data from our second Experiment in our paper
[`Testing the influence of QUDs on the occurrence of conditional perfection` ](https://journals.linguisticsociety.org/proceedings/index.php/ELM/article/view/5413).

# Experiment

1. Code Experiment

- first run 'npm install' to load all necessary packages
- then open file index.html in a browser to run the experiment locally

Magpie-code for the experiment is in js-files, named according to the order how
they are loaded in the index.html-file:

- 02_custom_functions.js: mostly experiment-related functions, constants related to stimuli
- 03_custom_views_templates.js: generate view-templates, called in 05_views.js
- 04_trials.js: generates actual data for experiment
- 05_views.js: generate views used in the experiment
- 06_main.js: main configuration-file for the experiment (prolific config etc.),
where all views are specified that will be loaded in the experiment

2. Stimuli
All stimuli-related code/data is is stored in 'stimuli/'.
- img/
contains all images used in the experiment. Subfolders group1/group2 contain the
test-stimuli, with antecedent-block being blue, respectively green. Which trials are shown in test and train trials is specified here, too (TEST_IDS, TRAIN_IDS)

- 01_config.js
This is the file where all configurations of the generated scenes are set,
e.g.the width/color of blocks, the mapping from prior of blocks to fall to their
position on the edge (proportion of block that is on top of platform), etc.

- index-stimuli.html
run this file to show generated animations (set MODE in stimuli/01_config.js to
'test' or 'train' depending on animations you want to see)

- The following kinds of **test-stimuli** are implemented:
  1. **if1_xy**
  x (y) correspond to prior probability of antecedent (consequent)-block to fall
  on its own. In these trials, the width of the base ramp (which is the platform
  on which the consequent-block stands) is always the same, as specified in
  stimuli/01_config.js in variable *IF1_BASE_RAMP* (default: high, this means
  that probability of lower block to fall is large if the ball rolls down,
  therefore high corresponds to a short platform, the longer the platform is,
  the more the probability of the block to fall will decrease).

  2. **if2_xyz**
  if2_xy: as if1_xy but with further third block on top right.

  3. **if2ssw_xy**
  x corresponds to the prior probability of the antecedent-block to fall on its
  own, i.e.,by the ball on the ramp that is rolling on its own, and y
  corresponds to the prior probability of the consequent-block to fall on its
  own. These trials are constructed such that the probability of the
  antecedent-block to fall depends on the width of the base ramp as the ball on
  top moves on its own, without the influence of anything else. Therefore, the
  position of the antecedent-block with respect to its base platform is always
  the same, as specified in stimuli/01_config.js in variable
  *IF2_POS_BLOCK_RAMP* (default: never, i.e. positioned completely on platform).

  4. **independent_xy**
  x corresponds to the prior probability of the antecedent-block (upper block), y to the prior probability of the consequent-block (lower block) to fall on its own, i.e.,by the position of the block on its platform (antecedent-block) or by the ball on the ramp that is rolling on its own (consequent-block). As in if2-trials, the width of the base ramp is defined by the prior probability of the consequent-block to fall, and the position of the consequent-block on that platform is always the same (completely on top of platform), as specified in stimuli/01_config.js in variable *IND_POS_BLOCK_RAMP* (default: never).

  5. **independent_edge_xy**
  x corresponds to the prior probability of the antecedent-block (left block), y
  to the prior probability of the consequent-block (right block) to fall on its
  own. Simple situations with two blocks, each on a separate platform.
