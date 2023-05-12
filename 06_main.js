// In this file you initialize and configure your experiment using magpieInit

$("document")
  .ready(function () {
    // prevent scrolling when space is pressed
    window.onkeydown = function (e) {
      if (e.keyCode === 32 && e.target === document.body) {
        e.preventDefault();
      }
    };

    // calls magpieInit
    // in debug mode this returns the magpie-object, which you can access in the console of your browser
    // e.g. >> window.magpie_monitor or window.magpie_monitor.findNextView()
    // in all other modes null will be returned
    window.magpie_monitor = magpieInit({
      // You have to specify all views you want to use in this experiment and the order of them
      //*alternating trials of Exp1 (prior elicitation) and Exp2 (production)* in views_test!//
      views_seq: [
      botcaptcha,
      intro,
      instructions_general,
      animation_view, // training phase
      //instructions_test_img_3player,
      instructions_test_img,
      instructions_practice,
      practice_image_selection,
      divider_practice_complete,
      test_image_selection_01,
      divider_first_break,
      test_image_selection_02,
      divider_finished,
      post_test_questions_experiment,
      post_test,
      thanks
      ],
      // Here, you can specify all information for the deployment
      deploy: {
        experimentID: "82",
        serverAppURL: "https://mcmpact.ikw.uni-osnabrueck.de/magpie/api/submit_experiment/",
        // Possible deployment methods are:
        // "debug" and "directLink"
        // As well as "MTurk", "MTurkSandbox" and "Prolific"
        deployMethod: "debug",
        contact_email: "britta.grusdt@uni-osnabrueck.de",
        prolificURL: "https://app.prolific.co/submissions/complete?cc=8185C464"
      },
      // Here, you can specify how the progress bar should look like
      progress_bar: {
        // list the view-names of the views for which you want a progress bar
        in: [test_image_selection_01.name, test_image_selection_02.name,
              animation_view.name, practice_image_selection.name],
      // Possible styles are "default", "separate" and "chunks"
        style: "default",
        width: 100
      }
    });
  });
