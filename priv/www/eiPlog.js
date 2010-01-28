var applications;
var valid_name = /^[-0-9a-zA-Z_]{3,255}$/;

function init(callback){
  applications = new Hash();
  new Ajax.Request("/applications", {
    method: 'get',
    onSuccess: function(transport){
      var apps = transport.responseJSON;
      var yet = apps.length;
      apps.each(function(app){
        applications.set(app, "waiting");
        new Ajax.Request("/events/"+app, {
          method: 'get',
          onSuccess: function(transport){
            handle_events(app, transport.responseJSON);
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
      var div = Builder.node("div", {id: "app_"+app, className: "appev"}, [app]);//,Builder.node("div",{id: "delete_"+app, className: "delete_button"}, "X")]);
      tdApps.appendChild(div);
      div.observe("click", click_app.curry(app));
      //$("delete_"+app).observe("click", function(ev){ev.stop();delete_app(app)});
  });
  var newAppDiv = Builder.node("div", {className: "appev appev_new"}, 
      [Builder.node("input", {id: "new_app_name", type: "text"}),
       Builder.node("button", {id: "new_app_button"}, "create")]);
  tdApps.appendChild(newAppDiv);
  $("new_app_button").observe("click", function(){
      var name = ($("new_app_name").value+"").strip();
      if(valid_name.exec(name)){
        new Ajax.Request("/applications/"+encodeURIComponent(name), {
          method: 'post', 
          contentType: 'text/plain',
          onFailure: function(t){alert("Error code "+t.status);},
          onSuccess: init
          });
      }
      else
        alert("Application names must consist of only letters, numbers, dashes, and underscores");
  });
}

function click_app(app){
  show_events(app)
  tdApps.select(".appev_highlight").each(function(el){el.removeClassName("appev_highlight");});
  $("app_"+app).addClassName("appev_highlight");
}

function delete_app(app){
  new Ajax.Request("/applications/"+app, {method: "DELETE", onComplete: init});
}

function show_events(app){
  var tdEvs = $("tdEvs");
  tdEvs.childElements().each(Element.remove);
  applications.get(app).each(function(ev){
      var div = Builder.node("div", {id: "ev_"+ev, className: "appev"}, ev);
      tdEvs.appendChild(div);
      div.observe("click", function(){
        load_event(app, ev);
        tdEvs.select(".appev_highlight").each(function(el){el.removeClassName("appev_highlight")});
        $("ev_"+ev).addClassName("appev_highlight");
      });
  });
  var newEvDiv = Builder.node("div", {className: "appev appev_new"}, 
      [Builder.node("input", {id: "new_ev_name", type: "text"}),
       Builder.node("button", {id: "new_ev_button"}, "create")]);
  tdEvs.appendChild(newEvDiv);
  $("new_ev_button").observe("click", function(){
      var name = ($("new_ev_name").value+"").strip();
      if(valid_name.exec(name)){
        new Ajax.Request("/events/"+encodeURIComponent(app)+"/"+encodeURIComponent(name), {
          method: 'post', 
          contentType: 'text/plain',
          onFailure: function(t){
            alert("Error code "+t.status);
          },
          onSuccess: function(transport){
            init(function(){
              click_app(app);
            });
          }});
      }
      else
        alert("Event names must consist of only letters, numbers, dashes, and underscores");
  });
}

function handle_events(app, events){
  applications.set(app, events);
}

function load_event(app, ev){
  get_logs(app, ev, 30, 1, "ASC");
}

function get_logs(app, ev, limit, page, order, context){
  var url = "/logs/"+app+"/"+ev+"?limit="+limit+"&page="+page+"&order="+order+"&key="+$("eiPlog_key").value;
  if(context)
    url = url+"&context="+encodeURIComponent(context);
  new Ajax.Request(url,
      {method: "GET", onSuccess: function(transport){
        show_logs(transport.responseJSON, context)}});
}

function show_logs(Ob, context){
  var logs = Ob.logs;
  var el = $("logs");
  el.childElements().each(Element.remove);
  logs.each(function(log){
    var div = Builder.node("div", {className: "log"}, [
      Builder.node("span", {className: "log_time"}, log.time),
      Builder.node("span", {className: "log_context"}, log.context || context),
      Buidler.node("span", {className: "log_details"}, log.details)]);
    el.appendChild(div);
  });
  if(logs.length == 0)
    el.appendChild(Builder.node("div", {className: "error"}, "No logs"));
}
