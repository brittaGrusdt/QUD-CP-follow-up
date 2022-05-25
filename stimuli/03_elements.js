 let Walls = {'test': {}, 'train': {}, 'tilted': {}};

// ground of scene
const Bottom = wall(label='bottom', x=SCENE.w/2, y=SCENE.h - PROPS.bottom.h/2,
  w=SCENE.w, h=PROPS.bottom.h);

//@arg prior: determines the width of the lower wall of the ramp
makeRamp = function(dir, prior, increase, w1, label1="bottom", test=true,
  col_ball=COLORS_BALL.test) {

  let angle = ANGLES.default;
  let overlap = OVERLAP_SHIFT["angle" + angle];
  let label2 = label1 === "bottom" ? "top" : "bottom";

  let dat = increase && label1 === "top" ? {rx: "min", ry: "min", x: -1, y: 1} :
            increase && label1 === "bottom" ? {rx: "max", ry: "max", x: 1, y: -1} :
            label1 === "top" ? {rx: "max", ry: "min", x: 1, y: 1}
                             : {rx: "min", ry: "max", x: -1, y: -1};
  // 1. sin(angle) = h/w_tillted and 2. h² + w_low² = ramp²
  let r = increase ? radians(360 - angle) : radians(angle);
  // always use the same angle + width of the ramp!
  let factor = 1.2
  // let factor = dir == "horizontal" ? 1.5 : 0.9;
  let ramp_width = factor * (Math.sqrt(Math.pow(100, 2) / (1 - Math.pow(Math.sin(r), 2))));
  let ramp = wall('ramp' + angle, w1.bounds[dat.rx].x + dat.x * ramp_width/2,
    w1.bounds[dat.ry].y + dat.y * PROPS.walls.h/2, ramp_width);
  Body.rotate(ramp, r, {x: w1.bounds[dat.rx].x, y: w1.bounds[dat.ry].y});

  let width_w2 = label1 == "bottom" ? BASE_RAMP.default : BASE_RAMP[dir][prior]
  let x2 = dat.x == 1 ? "max" : "min";
  let y2 = dat.y == 1 ? "max" : "min";
  let w2 = wall(label = 'ramp_' + label2 + angle,
    ramp.bounds[x2].x + dat.x * width_w2/2 - dat.x * overlap,
    ramp.bounds[y2].y - dat.y * PROPS.walls.h/2, width_w2);
  dat.walls = label1==="bottom" ? {'top': w2, 'bottom': w1} : {'top': w1, 'bottom': w2};
  dat.x_ball = increase ? dat.walls.top.bounds.min.x - PROPS.balls.move_to_roll : dat.walls.top.bounds.max.x + PROPS.balls.move_to_roll;
  let ball1 = ball(dat.x_ball, dat.walls.top.bounds.min.y - PROPS.balls.radius,
    PROPS.balls.radius, 'ball1', col_ball);

  return {'tilted': ramp, 'wall_top': dat.walls.top,
   'wall_bottom': dat.walls.bottom, 'ball': ball1, 'angle': angle}
}

seesaw = function(x, y_base_min=SCENE.h - PROPS.bottom.h, props={}){
  props = Object.keys(props).length == 0 ? PROPS.seesaw :
    Object.assign({}, PROPS.seesaw, props);
  let y = y_base_min - props.stick.h / 2;
  let stick = wall('stick', x, y,
    props.stick.w, props.stick.h, {render: {fillStyle: COLS.darkgrey}});

  let link = wall('link', x, stick.bounds.min.y - props.link.h/2,
    props.link.w, props.link.h, {render: {fillStyle: COLS.darkbrown}}
  );
  let skeleton = Body.create({'parts': [stick, link],
    'isStatic': true,
    'label': 'skeleton'
  });
  let def_ss = props;
  let y_plank = link.bounds.min.y - def_ss.plank.h/2;
  let opts = Object.assign({label:'plank', render:{fillStyle: COLS_OBJS_HEX.plank[0]}}, OPTS.plank);
  let plank = Bodies.rectangle(x, y_plank, def_ss.plank.w, def_ss.plank.h, opts)

  var constraint = Constraint.create({
    pointA: {x: plank.position.x, y: plank.position.y},
    bodyB: plank,
    stiffness: 0.7,
    length: 0
  });
  return {stick, link, skeleton, plank, constraint}
}

// The first two list entries are respectively the bases for block1 and block2
wallsIf1 = function(side, horiz, prior, exhaustive, with_ramp=true){
  let x_up_r = 700
  let x_up_l = 100
  let base_ssw_y = SCENE.h - 215
  let y_up = base_ssw_y - 95
  // use to move antecedent-block up
  //y_up = base_ssw_y - 175
  let width_wall_up = 150
  let dat = side == "right" ?
    {w_up: wall('wall_ac_up', x_up_r, y_up, width_wall_up), x_up: x_up_r, move_x: 1, increase: true} :
    {w_up:  wall('wall_ac_up', x_up_l, y_up, width_wall_up), x_up: x_up_l, move_x: -1, increase: false};
  let base_ssw = wall('base_seesaw', dat.x_up+225*(-dat.move_x), base_ssw_y, PROPS.if1_base_ssw.w,
                      PROPS.if1_base_ssw.h);
  let x = base_ssw.position.x + dat.move_x * 40 + dat.move_x * PROPS.walls.w/2
  let y = base_ssw.position.y + 67;
  let ramp_top = wall('ramp_top', x, y);
  let ramp = makeRamp(horiz, prior, dat.increase, ramp_top, "top")
  Body.setPosition(ramp.ball, {x: ramp.ball.position.x + 40 * dat.move_x,
    y: ramp.ball.position.y});
  let ssw = seesaw(base_ssw.position.x, base_ssw.bounds.min.y, PROPS.if1_ssw);

  let wall_exhaustive = []
  if(exhaustive == false){
    let x_w_up2 =  ssw.plank.position.x + (-1 * dat.move_x) * (PROPS.if1_ssw.plank.w/2 +
    PROPS.seesaw.d_to_walls + width_wall_up/2)
    let y_up_ex = dat.w_up.position.y - PROPS.blocks.h
    // use this to keep yellow block where it was (not to move with respect to antecedent-block)
    //y_up_ex = base_ssw_y - 95 - PROPS.blocks.h;
    let w_ex = wall('wall_ac_up2', x_w_up2, y_up_ex, width_wall_up)
    wall_exhaustive.push(w_ex);
  }
  let base_walls = [dat.w_up, ramp.wall_bottom].concat(wall_exhaustive);
  let other_walls = with_ramp ? [ramp_top, ramp.tilted, base_ssw, ssw.skeleton]
                              : [ramp_top, base_ssw, ssw.skeleton];

  return {walls: base_walls.concat(other_walls),
          dynamic: [ramp.ball, ssw.plank, ssw.constraint]}
}

seesawIf2 = function(prior, dir, side_ramp, offset=PROPS.seesaw.d_to_walls){
  let y_low = SCENE.h - 180; //220;
  let y_high = y_low - 50; //170;
  let w_w2blocks = PROPS.seesaw.base2blocks.w;
  let data = side_ramp === "right" ?
    {x0: 75, y0: y_low, w0: w_w2blocks, y1: y_high, w1: BASE_RAMP[dir][prior]} :
    {x0: 300, y0: y_high, w0: BASE_RAMP[dir][prior], y1: y_low, w1: w_w2blocks};
  let base0 = wall('seesaw_base_left', data.x0, data.y0, data.w0);
  let pos = base0.bounds.max.x + PROPS.seesaw.plank.w/2 + offset;
  let objs = seesaw(pos);
  let base1 = wall('seesaw_base_right',
    objs.plank.bounds.max.x + offset + data.w1/2, data.y1, data.w1);
  let walls = [base0, base1].concat([objs.skeleton]);
  return {'walls': walls, 'dynamic': [objs.plank, objs.constraint]}
}

Walls.test = {
  'independent': [[wall('w_up1', 280, SCENE.h-300), wall('w_low1', 290, SCENE.h-150, w=100)],
                  [wall('w_up2', 520, SCENE.h-300), wall('w_low2', 510, SCENE.h-150, w=100)]],
  // 'independent_edge': [wall('w_up1', 300, SCENE.h-200, w=100), wall('w_low1', 500, SCENE.h-200, w=100)],
  'independent_edge': [wall('w_up1', 150, SCENE.h-200, w=100), wall('w_low1', 400, SCENE.h-200, w=100)],
  'independent_edge3': [wall('w_up1', 150, SCENE.h-200, w=100),
                       wall('w_low1', 400, SCENE.h-200, w=100),
                       wall('w_ex', 650, SCENE.h-200, w=100)],
  'if1': wallsIf1,
  'if2ssw': seesawIf2
};

//// Elements for TRAINING TRIALS //////
Walls.train.uncertain = [
  wall('w_mid_left', 0.3 * SCENE.w, SCENE.h/2, PROPS.seesaw.base2blocks.w),
  wall('w_mid_right', 0.75 * SCENE.w, SCENE.h/2, PROPS.seesaw.base2blocks.w)
];

Walls.train.steepness = [wall('w_bottom1', SCENE.w/2, 100),
  wall('w_bottom2', SCENE.w/2, 220),
  wall('w_bottom3', SCENE.w/2, 340)];

Walls.train.distance0 = [
  wall('w_top1', 150, 50),
  wall('w_top2', 150, 160),
  wall('w_top2', 150, 280)
];

Walls.train.distance1 = [
  wall('w_top1', 150, 50),
  wall('w_top2', 150, 160),
  wall('w_top3', 150, 280)
];

Walls.train.independent = [
  wall('ramp_top', 100, 75),
  wall('w_right', 750, 140, 90)
];
Walls.train.if1 = wallsIf1

Walls.train.ssw = function(){
  let x = SCENE.w/2 - 30
  let pw = 280
  let objs = seesaw(x, SCENE.h - PROPS.bottom.h,
    props={'plank': {'w': pw, 'h': 10}});

  let bw = PROPS.seesaw.base2blocks.w
  let walls = [
    wall('wallTopLeft', 150, 155, 140),
    wall('wall_seesaw_right', x+pw/2+PROPS.seesaw.d_to_walls+bw/2, 240, bw)
  ].concat([objs.skeleton]);
  return {'walls': walls, 'dynamic': [objs.plank, objs.constraint]}
}

// moving triangle that appears in animations s.t. participants see that the
// animation runs if nothing happens
movingIrrelevantObj = function(){
  var x = 120;
  var y = 430;
  var body = Bodies.polygon(x, y, 3, 20,
    {render: {fillStyle: COLS.pink_red}});

  var constraint = Constraint.create({
      pointA: { x: x, y: y-30},
      bodyB: body,
      stiffness: 0.002,
      render: {
         strokeStyle: COLS.olive,
         lineWidth: 3
    }
  });

  return([body, constraint]);
}
