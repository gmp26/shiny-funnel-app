HTMLWidgets.widget({

  name: 'asquare',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        // TODO: code to render the widget, e.g.
        // el.innerText = x.message;
        // mountComponent(el, x.fill);
        cljsWidgets.filled_square(el, x.fill);
        console.log("x");
        console.log(x);
      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
