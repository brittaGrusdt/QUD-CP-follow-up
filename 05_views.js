// In this file you can instantiate your views
// We here first instantiate wrapping views, then the trial views

/** Wrapping views below

* Obligatory properties

    * trials: int - the number of trials this view will appear
    * name: string

*Optional properties
    * buttonText: string - the text on the button (default: 'next')
    * text: string - the text to be displayed in this view
    * title: string - the title of this view

    * More about the properties and functions of the wrapping views - https://magpie-ea.github.io/magpie-docs/01_designing_experiments/01_template_views/#wrapping-views

*/

// Every experiment should start with an intro view. Here you can welcome your participants and tell them what the experiment is about
const intro = magpieViews.view_generator("intro", {
  trials: 1,
  name: "intro",
  // If you use JavaScripts Template String `I am a Template String`, you can use HTML <></> and javascript ${} inside
  text: `Thank you for your participation in our study!
         Your anonymous data makes an important contribution to our understanding of human language use.
          <br />
          <br />
          Legal information:
          By answering the following questions, you are participating in a study
          being performed by scientists from the University of Osnabrueck.
          <br />
          <br />
          You must be at least 18 years old to participate.
          <br />
          <br />
          Your participation in this research is voluntary.
          You may decline further participation, at any time, without adverse consequences.
          <br />
          <br />
          Your anonymity is assured; the researchers who have requested your
          participation will not receive any personal information about you.
          `,
  buttonText: "begin the experiment"
});

const instructions_general = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_general",
  title: "General Instructions",
  text: `This experiment consists of two phases, a training and a testing phase.
         In total, you will need approximately <b>10 minutes</b> to finish.
         <br/>
         <br/>
         We will start with the <b>training phase</b> which consists of <b>8 trials</b> in which you will see scenes of block arrangements.
         Your task is to select all blocks that you think will fall.
        <br/>
         After you made your selection, click on RUN to see which blocks actually fall.
         Note that in the lower left, you will see a little moving triangle, which is just there for you to see that the animation is running.
         Then, click on CONTINUE to proceed with the next training trial.<br/><br/>
         <b>Please note</b>:
         <br/>
         We recommend to use <b>Full Screen Mode</b> throughout the experiment (usually switched on/off with F11, on a Mac press Command+Control+F).`,
  buttonText: "Start Training"
});

// Image choices: anns-bobs-screen.jpeg, all-one-row, separate-ann-bob, separate-you

const instructions_test_img_3player = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_test_img",
  title: "Instructions Testing Phase",
  text: `Great -  we'll now proceed with the test phase of the experiment.<br/><br/>
  You will play a simple 3-player game with Ann and Bob in which you have to work together as a team.
  This is an example of how the game may look like for each of you:

  <br/>
    <img src="./stimuli/img/screens-instruction/separate-ann-bob.png" style="max-width:90%">
    <img src="./stimuli/img/screens-instruction/separate-you.png" style="max-width:90%">
  <br/><br/>

  Ann's, Bob's and your joint goal is to make as many points as possible.
  During the game, only a minimal amount of interaction is allowed: <br/><br/>

  <b>1.</b> Ann, who does not have any experience with these scenes of block arrangements, sees part of the scene that Bob sees,
  but she can ask Bob a question about it.<br/><br/>

  <b>2.</b> Bob, who is an expert concerning these scenes of block arrangements, can only reply to Ann's question to inform <i>you</i> about the scene that he sees.<br/><br/>

  <b>3.</b> You will see the partial scene that Ann sees, the question she asked and Bob's response.
  Based on this information, your task will be to decide which of three shown scenes is the one that Bob sees.<br/><br/>

  You may select <b>one or more</b> of the three scenes shown to you.
  Depending on your choice, Ann, Bob and you will get or lose points.<br/>

  To demonstrate this, we use the example from above again:
  <br/><br/>
    <img src="./stimuli/img/screens-instruction/anns-bobs-screen.jpeg" style="max-width:90%">
  <br/><br/>

  <u>This is what they say</u>:<br/>
  <b>Ann</b>: Which blocks do you think will fall?<br/>
  <b>Bob</b>: The BLUE and the GREEN block will fall.<br/><br/>
  <b>Note: Please read both, Ann's question and Bob's answer, carefully!</b><br/><br/>

  <u>Number of points you get depending on your selection</u>:<br/>
  If you are confident and select a single scene, and your choice is correct,<br/>
  you will <b>get 100 points</b>:.
  <br/>
  <img src="./stimuli/img/screens-instruction/select1-true.png" style="max-width:90%">
  <br/><br/>
  However, if you select a single scene and your choice is wrong,<br/>
  you will <b>lose 100 points</b>:<br/>
  <img src="./stimuli/img/screens-instruction/select1-false.png" style="max-width:90%">
  <br/><br/>
  But if you select two scenes, including the correct one, you will <b>win 50 points</b>:<br/>
  <img src="./stimuli/img/screens-instruction/select2.png" style="max-width:90%">
  <br/><br/>

  Therefore, in order to avoid loosing points, it will be better for you to select two scenes when you are undecided.<br/>

  Even though this is unlikely to happen, note that you will also <b>lose 100 points</b> if:<br/>
  1. you select two scenes, <i>not</i> including the correct one OR if<br/>
  2. you select all three scenes.<br/><br/>`,
  buttonText: "continue"
});

const instructions_test_img = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_test_img",
  title: "Instructions Testing Phase",
  text: `Great -  we'll now proceed with the test phase of the experiment in
  which you will play a simple game.<br/><br/>

  There are two persons, Ann and Bob, sitting in front of two separate screens.
  While Bob sees an entire scene of block arrangements, Ann only sees half of the
  same scene on her screen, the other half is occluded, for example:
  <br/><br/>
    <img src="./stimuli/img/screens-instruction/anns-bobs-screen.jpeg" style="max-width:90%">
  <br/><br/>
  To get more information about the scene, Ann can ask Bob a single question.
  Since Ann and Bob are close friends, he tries to help her as good as he can with his answer.
  <br/><br/>
  You will see three scenes of block arrangements and based on (i) Ann's
  question and (ii) Bob's answer your task will be to decide which of the scenes
  Ann and Bob see (Bob completely, Ann only partially). Depending on your level
  of confidence, you can select one or more of the three scenes, if you think
  that more than one could fit. The number of scenes that you select influences
  how many points you will get. For example, consider Ann's and Bob's screen
  from above and the following conversation between them.<br/><br/>

  <u>Ann's question to get more information</u>:
  <b>${QUESTS.neutral}</b><br/>
  <u>Bob aims to help Ann and replies</u>:
  <b>${ANSWERS.blue_green}</b><br/>
  <br/>

  If you are confident, select a single scene, and your choice is right,
  you <b>get 100 points</b>:
  <br/>
  <img src="./stimuli/img/screens-instruction/select1-true.png" style="max-width:90%">
  <br/><br/>
  However, if you select a single scene and your choice is wrong, you <b>lose 100 points</b>:<br/>
  <img src="./stimuli/img/screens-instruction/select1-false.png" style="max-width:90%">
  <br/><br/>
  But if you select two scenes, including the correct one, you will <b>win 50 points</b>:<br/>
  <img src="./stimuli/img/screens-instruction/select2.png" style="max-width:90%">
  <br/><br/>

  Therefore, in order to avoid loosing points, it will be better for you to select two scenes when you are undecided.<br/>

  Even though this is unlikely to happen, note that you will also <b>lose 100 points</b> if:<br/>
  1. you select two scenes, <i>not</i> including the correct one OR if<br/>
  2. you select all three scenes.<br/><br/>`,
  buttonText: "continue"
});

const instructions_practice = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_practice",
  title: "Information Testing Phase",
  text: `The testing phase consists of <b>3 blocks</b>. <br/>
  The <b>1<sup>st</sup> block</b> is a <b>practice phase</b> with <b>4 trials</b> that should familiarize you with the task.<br/>
  The <b>following 2 blocks</b> build the <b>experimental phase</b> with a total of <b>13 trials</b>.<br/><br/>
  After each block, you will have the possibility to take a break.<br/><br/>

  Note that in the experimental phase, there will be a straight-forward attention-check trial, in which you will be explicitly told which picture to select.
  This helps us to make sure that the experiment was done carefully.
  Please note that if you fail to respond correctly in the attention-check trial, we will not be able to pay you.</br></br>

  When you're ready, please, click on CONTINUE to start the practice phase.`,
  buttonText: "continue"
});

const post_test = magpieViews.view_generator("post_test", {
  trials: 1,
  name: "post_test",
  title: "Additional information",
  text: "Answering the following questions is optional, but your answers will help us analyze our results.",
  comments_question: 'Further comments'
});

const post_test_questions_experiment = magpieViews.view_generator("post_test", {
  trials: 1,
  name: "post_test_question_ann",
  title: "Questions",
  text: `We have a few more questions concerning the experiment.</br>
  Please select all statements that you agree with.`
},
  {stimulus_container_generator: post_questions_generator.stimulus_container_gen,
   answer_container_generator: post_questions_generator.answer_container_gen,
   handle_response_function: post_questions_generator.handle_response_fn
  }
);

// The 'thanks' view is crucial; never delete it; it submits the results!
const thanks = magpieViews.view_generator("thanks", {
  trials: 1,
  name: "thanks",
  title: "Thank you very much for taking part in this experiment!",
  prolificConfirmText: "Press the button"
});

const animation_view = animation_generator(
  {name: "animation_view",
   title: "title",
   CT: 0,
   trials: NB_TRAIN_TRIALS,
   data: ""}
 )

const practice_image_selection = magpieViews.view_generator(
  "image_selection", {
    data: PRACTICE_TRIALS,
    name: 'image_selection',
    trials: PRACTICE_TRIALS.length
  }, {
    stimulus_container_generator: forced_choice_generator.stimulus_container_gen,
    answer_container_generator: forced_choice_generator.answer_container_gen,
    handle_response_function: forced_choice_generator.handle_response_function
  }
);

const test_image_selection_01 = magpieViews.view_generator(
  "image_selection", {
    data: TEST_TRIALS_01,
    name: 'image_selection_01',
    trials: TEST_TRIALS_01.length
  }, {
    stimulus_container_generator: forced_choice_generator.stimulus_container_gen,
    answer_container_generator: forced_choice_generator.answer_container_gen,
    handle_response_function: forced_choice_generator.handle_response_function
  }
);

const test_image_selection_02 = magpieViews.view_generator(
  "image_selection", {
    data: TEST_TRIALS_02,
    name: 'image_selection_02',
    trials: TEST_TRIALS_02.length
  }, {
    stimulus_container_generator: forced_choice_generator.stimulus_container_gen,
    answer_container_generator: forced_choice_generator.answer_container_gen,
    handle_response_function: forced_choice_generator.handle_response_function
  }
);

const divider_practice_complete = custom_text_scores({
  name: "divider_practice_complete", title: "Practice finished",
  text: {prescore: `In the practice phase`,
         postscore: `Note that in the following, you will not get feedback anymore.
         But you will be told your final score in the end of the experiment.</br></br>
         When you are ready, please click on CONTINUE to start the first block of the
    experimental trials.`},
  trials: 1,
  buttonText: "continue"
});

const divider_first_break = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "divider_first_break",
  title: "Take a short break",
  text: `Well done so far! You can now take a short break if needed. As soon as
   you are ready to continue with the experiment, click on the button below.`,
  buttonText: "Continue with next block"
});

const divider_finished = custom_text_scores({
  name: "divider_finished", title: "Test Phase finished",
  text: {prescore: `For your interest: in the test phase`,
         postscore: `Please click on NEXT to continue.`},
  trials: 1,
  buttonText: "next"
});

// captcha
// speaker and listeners names to be sampled from for the botcaptcha
var speaker = _.sample(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"]);
var listener = _.sample(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"]);

const botcaptcha = custom_botcaptcha({
  name: 'botcaptcha',
  trials: 1,
  story: speaker + ' says to ' + listener + ': "It\'s a beautiful day, isn\'t it?"',
  question: "Who is " + speaker + " talking to?",
  speaker: speaker,
  listener: listener
});
