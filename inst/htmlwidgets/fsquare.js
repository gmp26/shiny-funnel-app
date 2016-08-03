HTMLWidgets.widget({
  
  name: 'fsquare',
  
  type: 'output',
  
  factory: function(el, width, height) {
    
    // TODO: define shared variables for this instance
    
    return {
      
      renderValue: function(x) {
        
        console.log("x = ");
        console.log(x);
        // TODO: code to render the widget, e.g.
        // el.innerText = x.message;
        window.mountComponent(el, x.fill);
      },
      
      resize: function(width, height) {
        
        // TODO: code to re-render the widget with a new size
        
      }
      
    };
  }
});
