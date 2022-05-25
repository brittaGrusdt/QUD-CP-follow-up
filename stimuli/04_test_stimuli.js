let TestStimuli = {"independent": {}, "if1": {}, "if2ssw": {}, 'if2': {},
                   "independent_edge": {}, "independent_edge3": {},
                   "if1_ind": {}};

// IMPORTANT: DYNAMIC BLOCKS HAVE TO BE ADDED BEFORE STATIC OBJECTS, OTHERWISE
// THEY WILL FALL VERY ODD (JITTERING)

testTrials_if2ssw = function(priors){
  let pp = priors[0][0] + priors[1][0]
  let horiz = HORIZ_AC2[pp];
  // specify trials for which ramp is on right and for which on left of scene
  // ramp-width is based on what is stored here in prior, for if2ssw-trials, we
  // want its width to be dependent on the prior of the block on the base wall of
  // of the ramp
  let data = priors[0] !== priors[1] ?
    {side_ramp: "left", i_ramp: 0, b_sides: [1,1], prior: priors[0], 'increase': 0}:
    {side_ramp: "right", i_ramp: 1, b_sides: [-1,-1], prior: priors[1], 'increase': 1};

  let seesaw = Walls.test.if2ssw(data.prior, horiz[data.i_ramp], data.side_ramp)
  let ramp = makeRamp(horiz[data.i_ramp], priors[data.i_ramp], data.increase,
    seesaw.walls[data.i_ramp]);
  let objs = seesaw.dynamic.concat([ramp.ball]);
  objs = objs.concat([ramp.tilted, ramp.wall_top]).concat(seesaw.walls);
  let w_no_ramp = seesaw.walls[data.i_ramp === 0 ? 1 : 0];
  // let w_x = w_no_ramp.bounds[data.i_ramp === 0 ? "min" : "max"].x
  // let w_width = w_no_ramp.bounds.max.x - w_no_ramp.bounds.min.x;
  let xBlock = blockOnBase(w_no_ramp, -1 * data.b_sides[0] * 0.60,
    COLS.sienna, "Xblock", true);
  let bases = [xBlock];
  let w_ramp = seesaw.walls[data.i_ramp];
  data.i_ramp === 0 ? bases.unshift(w_ramp) : bases.push(w_ramp);
  let blocks = [];
  let colors = assignColors();
  let c1 = COLS_OBJS_HEX.test_blocks[colors[0]];
  let c2 = COLS_OBJS_HEX.test_blocks[colors[1]];

  let dir = horiz[data.i_ramp]
  let w = dir == 'horizontal' ? PROPS.blocks.h : PROPS.blocks.w;
  let p_fall = priors[data.i_ramp]

  // in if2ssw-trials the prior for the antecedent-block to fall is defined by the
  // width of the base ramp wall. The block itself is always positioned such that
  // according to its position, it would never fall on its own.
  let ps = [data.i_ramp === 0 ? PRIOR[horiz[0]][IF2_POS_BLOCK_RAMP] : PRIOR[dir][priors[0]],
            data.i_ramp === 1 ? PRIOR[horiz[1]][IF2_POS_BLOCK_RAMP] : PRIOR[dir][priors[1]]];
  let b1 = blockOnBase(bases[0], data.b_sides[0] * ps[0], c1, 'blockA', horiz[0]=='horizontal');
  let b2 = blockOnBase(bases[1], data.b_sides[1] * ps[1], c2, 'blockC', horiz[1]=='horizontal');

  if(p_fall == "low" && data.i_ramp == 0 && horiz[0]=="horizontal"){
      Matter.Body.setPosition(b1, {'x': b1.position.x - DIST_POS_BLOCK_RAMP_LOW,
                                   'y': b1.position.y})
  } else if(p_fall == "low" && horiz[1] == "horizontal") {
      Matter.Body.setPosition(b2, {'x': b2.position.x + DIST_POS_BLOCK_RAMP_LOW,
                                   'y': b2.position.y});
  }

  let twoBlocks = [b1, b2];
  blocks = twoBlocks.concat(blocks);
  return blocks.concat(xBlock).concat(objs);
}

testTrials_if = function(priors, nb_causes, with_ramp=true){
  let colors = assignColors();
  let p1 = priors[0];
  let p2 = priors[1];
  let horiz =  HORIZ_AC1[p1[0]+p2[0]];
  let b2_w = horiz[1] == 'horizontal' ? PROPS.blocks.h : PROPS.blocks.w;

  let data = ['ll', 'hl'].includes(p1[0]+p2[0]) ?
  {edge_blocks: -1, increase: true, idx_w: 0, moveBall: 1, side:"right"} :
  {edge_blocks: 1, increase: false, idx_w: 1, moveBall: -1, side:"left"};

  let exhaustive = nb_causes == 1 ? true : false
  let objs = Walls.test.if1(data.side, horiz[1], IF1_BASE_RAMP, exhaustive, with_ramp)

  let b1 = blockOnBase(objs.walls[0], PRIOR[horiz[0]][p1] * data.edge_blocks,
    COLS_OBJS_HEX.test_blocks[colors[0]], 'blockA', horiz[0] == 'horizontal');
  let b2 =  blockOnBase(objs.walls[1], PRIOR[horiz[1]][p2] * data.edge_blocks,
    COLS_OBJS_HEX.test_blocks[colors[1]], 'blockC', horiz[1] == 'horizontal');

  let extra_block = [];
  if(nb_causes == 2){
    let pbx = priors[2]
    let bx = blockOnBase(objs.walls[2], PRIOR[horiz[0]][pbx] * (-1 * data.edge_blocks),
    COLS_OBJS_HEX.if2_xblock, 'blockX', horiz[0] == 'horizontal');
    // if extra-block has prior 'never', it is positioned in center of platform
    if(pbx == 'never'){
      Matter.Body.setPosition(bx, {'x': objs.walls[2].position.x,
                                   'y': bx.position.y});
    }
    extra_block.push(bx);
  }

  let blocks = [b1, b2].concat(extra_block).concat(objs.dynamic);
  return blocks.concat(objs.walls)
}

testTrials_independent_edge = function(priors, nBlocks=2) {
  let pp = priors[0][0] + priors[1][0];
  //specify trials with ramp on left and trials with ramp on right side
  let walls = nBlocks == 2 ? Walls.test.independent_edge : Walls.test.independent_edge3;
  let horiz = ['vertical', 'vertical', 'vertical']

  let colors = assignColors();
  let b1 = blockOnBase(walls[0], -1 * PRIOR[horiz[0]][priors[0]],
    COLS_OBJS_HEX.test_blocks[colors[0]], "blockA", horiz[0] == 'horizontal')
  let w2 = horiz[1] == 'horizontal' ? PROPS.blocks.h : PROPS.blocks.w;

  let ps = PRIOR[horiz[1]][IND_POS_BLOCK_RAMP]
  let b2 = blockOnBase(walls[1], PRIOR[horiz[1]][priors[1]],
    COLS_OBJS_HEX.test_blocks[colors[1]], "blockC", horiz[1] == 'horizontal')

  let blocks = [b1, b2]
  if(nBlocks == 3) {
    blocks = blocks.concat(
      [blockOnBase(walls[2], PRIOR[horiz[2]][priors[2]],
        COLS_OBJS_HEX.if2_xblock[0], "blockX", horiz[2] == 'horizontal')]
    )
  }

  return blocks.concat(walls);
}

testTrials_independent = function(priors){
  let pp = priors[0][0] + priors[1][0];
  //specify trials with ramp on left and trials with ramp on right side
  let data = ['ll', 'uh', 'hl'].includes(pp) ?
    {walls: Walls.test.independent[0], increase: false, sides: [-1, 1]} :
    {walls: Walls.test.independent[1], increase: true, sides: [1, -1]};

  let horiz = HORIZ_IND[pp];
  let ramp = makeRamp(horiz[1], priors[1], data.increase, data.walls[1], "top");

  let colors = assignColors();
  let b1 = blockOnBase(data.walls[0], data.sides[0] * PRIOR[horiz[0]][priors[0]],
    COLS_OBJS_HEX.test_blocks[colors[0]], "blockA", horiz[0] == 'horizontal')
  let w2 = horiz[1] == 'horizontal' ? PROPS.blocks.h : PROPS.blocks.w;

  let ps = PRIOR[horiz[1]][IND_POS_BLOCK_RAMP]
  let b2 = blockOnBase(ramp.wall_bottom, data.sides[1] * ps,
    COLS_OBJS_HEX.test_blocks[colors[1]], "blockC", horiz[1] == 'horizontal')
  if(priors[1] == "low" && !data.increase && horiz[1]=="horizontal"){
        Matter.Body.setPosition(b2, {'x': b2.position.x - DIST_POS_BLOCK_RAMP_LOW,
                                     'y': b2.position.y})
  } else if(priors[1] == "low" && data.increase && horiz[1] == "horizontal") {
        Matter.Body.setPosition(b2, {'x': b2.position.x + DIST_POS_BLOCK_RAMP_LOW,
                                     'y': b2.position.y});
  }
  // add seesaw as distractor
  let dist = seesaw(data.increase ? 680 : 120)
  let objs = data.walls.concat([dist.skeleton]).concat(
    [ramp.wall_top, ramp.wall_bottom, ramp.tilted]
  );
  return [b1, b2, ramp.ball].concat([dist.plank, dist.constraint]).concat(objs);
}

_prior2ID = function(prior_str){
  let abbrev = prior_str.endsWith("L") || prior_str.endsWith("H") ?
    (_.first(prior_str) + "-" + _.last(prior_str)) : _.first(prior_str);
  return abbrev
}

makeTestStimuli = function(conditions, relations, objStoredTo){
  relations.forEach(function(rel){
    let priors_all = conditions[rel]
    for(var i=0; i<priors_all.length; i++){
      let priors = priors_all[i];
      let pb1 = _prior2ID(priors[0])
      let pb2 = _prior2ID(priors[1])
      let pb3 = rel === "if2" || rel=="independent_edge3" ? _prior2ID(priors[2]) : ''

      let id = rel + '_' + pb1 + pb2 + pb3;
      let blocks = rel === "if2ssw" ? testTrials_if2ssw(priors) :
                   rel === "if1" ? testTrials_if(priors, 1) :
                   rel === "independent" ? testTrials_independent(priors) :
                   rel === "independent_edge" ? testTrials_independent_edge(priors) :
                   rel === "independent_edge3" ? testTrials_independent_edge(priors, 3) :
                   rel === "if2" ? testTrials_if(priors, 2) :
                   rel === "if1_ind" ? testTrials_if(priors, 1, false): null;

      objStoredTo[rel][id] = {"objs": blocks, "meta": priors, "id": id};
    }
  });
  return objStoredTo
}

if (MODE === "test") {
  let prior_conditions = getConditions();
  TestStimuli = makeTestStimuli(prior_conditions, TEST_TYPES, TestStimuli);
}
