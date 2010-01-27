var applications;

function init(callback){
  applications = new Hash();
  new Ajax.Request("/applications", {
    method: 'get',
    onSuccess: function(transport){
      var apps = eval("("+transport.responseText+")");
      var yet = apps.length;
      apps.each(function(app){
        applications.set(app, "waiting");
        new Ajax.Request("/events/"+app, {
          method: 'get',
          onSuccess: function(transport){
            handle_events(app, eval("("+transport.responseText+")"));
            if((--yet)==0 && callback)callback();
          }});
      });
      show_apps();
    }});
}


document.observe("dom:loaded", init);

function get_events(app){
}

function show_apps(){
  var tdApps = $("tdApps");
  $("tdEvs").childElements().each(Element.remove);
  tdApps.childElements().each(Element.remove);
  applications.keys().each(function(app){
      var div = Builder.node("div", {id: "app_"+app, className: "appev"}, app);
      tdApps.appendChild(div);
      div.observe("click", click_app.curry(app));
  });
  var newAppDiv = Builder.node("div", {className: "appev", style: "background-color:inherit;"}, 
      [Builder.node("input", {id: "new_app_name", type: "text"}),
       Builder.node("button", {id: "new_app_button"}, "create")]);
  tdApps.appendChild(newAppDiv);
  $("new_app_button").observe("click", function(){
      var name = ($("new_app_name").value+"").strip();
      if(name.length > 0){
        new Ajax.Request("/applications/"+name, {
          method: 'post', 
          onComplete: init
          });
      }
  });
}

function click_app(app){
  show_events(app)
  tdApps.select(".appev_highlight").each(function(el){el.removeClassName("appev_highlight");});
  $("app_"+app).addClassName("appev_highlight");
}

function show_events(app){
  var tdEvs = $("tdEvs");
  tdEvs.childElements().each(Element.remove);
  applications.get(app).each(function(ev){
      var div = Builder.node("div", {className: "appev"}, ev);
      tdEvs.appendChild(div);
  });
  var newEvDiv = Builder.node("div", {className: "appev", style: "background-color:inherit;"}, 
      [Builder.node("input", {id: "new_ev_name", type: "text"}),
       Builder.node("button", {id: "new_ev_button"}, "create")]);
  tdEvs.appendChild(newEvDiv);
  $("new_ev_button").observe("click", function(){
      var name = ($("new_ev_name").value+"").strip();
      if(name.length > 0){
        new Ajax.Request("/events/"+app+"/"+name, {
          method: 'post', 
          onComplete: function(){
            init(function(){
              click_app(app);
            });
          }});
      }
  });
}

function handle_events(app, events){
  applications.set(app, events);
}
