var applications;
var pageStack;
var valid_name = /^[-0-9a-zA-Z_]{3,255}$/;
var cal_after, cal_before;

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


document.observe("dom:loaded", function(){init();setup_cals();});

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
  clear_results();
}

function clear_results(){
  $("logs").childElements().each(Element.remove);
  $("total_logs_field").innerHTML="";
  $("page_num").innerHTML="";
  $("new_results_warning").hide();
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
        tdEvs.select(".appev_highlight").each(function(el){el.removeClassName("appev_highlight")});
        $("ev_"+ev).addClassName("appev_highlight");
        reload();
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

function get_selected_event(){
  var a = $("tdEvs").select(".appev_highlight");
  if(a.length > 0)return a.first().innerHTML;
}

function get_selected_app(){
  var a = $("tdApps").select(".appev_highlight");
  if(a.length > 0)return a.first().innerHTML;
}

function reload(pk){
  if(!pk)
    pageStack = {};
  var order = "ASC";
  if($("order_desc").checked)order = "DESC";
  get_logs($("results_per_page").value, order, pk);
}

function get_logs(limit, order, pageKey){
  var context = $("eiPlog_context").value;
  if(context.length==0)context=undefined;
  var app = get_selected_app();
  var ev = get_selected_event();
  var before = get_before();
  var after = get_after();
  if(!app || !ev)return;
  var params = $H({limit:limit, order: order, key: $("eiPlog_key").value});
  if(pageKey)
    params.set("page_key", pageKey);
  if(context)
    params.set("context", context);
  if(before)
    params.set("before", before);
  if(after)
    params.set("after", after);
  var url = "/logs/"+app+"/"+ev+"?"+params.toQueryString();
  clear_results();
  $("bprev").disabled=true;
  $("bnext").disabled=true;
  new Ajax.Request(url,
      {method: "GET", onSuccess: function(transport){
        show_logs(transport.responseJSON, context, limit, pageKey)},
        onFailure: function(transport){alert("Request failed with status: "+transport.status);}});
}

function show_logs(Ob, context, limit, page){
  if(Ob.error){
    alert("Error: "+Ob.error);
    return;
  }
  var logs = Ob.logs;
  var el = $("logs");
  if(logs.length == 0)
    el.appendChild(Builder.node("div", {className: "error"}, "No logs"));
  else
    el.appendChild(Builder.node("table", Builder.node("tbody",
      logs.map(function(log){
        return Builder.node("tr", {className: "log"}, [
          Builder.node("td", {className: "log_time"}, log.time),
          Builder.node("td", {className: "log_context"}, log.context || context),
          Builder.node("td", {className: "log_details"}, log.details)]);
      }))));

  $("total_logs_field").innerHTML = Ob.total;
  $("bprev").disabled=!Ob.prev_page;
  $("bnext").disabled=!Ob.next_page;
  pageStack.next=Ob.next_page;
  pageStack.prev=Ob.prev_page;
}

function go_back(){
  reload(pageStack.prev);
}

function go_forward(){
  reload(pageStack.next);
}

// Google calendar widgets
function setup_cals(){
  cal_before = new goog.ui.DatePicker(new goog.date.Date()); 
  cal_before.create($("calendar_before"));
  cal_after = new goog.ui.DatePicker(new goog.date.Date()); 
  cal_after.create($("calendar_after"));
  [cal_before, cal_after].each(function(cal){
      cal.setUseNarrowWeekdayNames(true);
      cal.setUseSimpleNavigationMenu(true);
      cal.setShowWeekNum(false);
      cal.setAllowNone(false);
  });
}

function get_before(){
  return before_after("check_before", cal_before, "hour_before", "minute_before", "235959", "59");
}

function get_after(){
  return before_after("check_after", cal_after, "hour_after", "minute_after", "000000", "00");
}

function before_after(checkbox, cal, hour, minute, default_time, seconds){
  if(!$(checkbox).checked)return;
  ret = cal.getDate().toString();
  var h = parseInt($(hour).value);
  var m = parseInt($(minute).value);
  if($R(0,23).include(h) && $R(0,59).include(m)){
    var hs = ((h < 10) ? "0" : "") + h;
    var ms = ((m < 10) ? "0" : "") + m;
    return ret + hs + ms + seconds;
  }
  else
    return ret + default_time;
}
