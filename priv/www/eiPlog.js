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
  clear_results();
}

function clear_results(){
  $("logs").childElements().each(Element.remove);
  $("total_logs_field").innerHTML="";
  $("page_links").childElements().each(Element.remove);
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

function reload(){
  var order = "ASC";
  if($("order_desc").checked)order = "DESC";
  get_logs($("results_per_page").value, 1, order);
}

function clicked_page(p){
  get_logs($("results_per_page").value, p, "ASC");
}

function get_logs(limit, page, order){
  var context = $("eiPlog_context").value;
  if(context.length==0)context=undefined;
  var app = get_selected_app();
  var ev = get_selected_event();
  if(!app || !ev)return;
  var url = "/logs/"+app+"/"+ev+"?limit="+limit+"&page="+page+"&order="+order+"&key="+$("eiPlog_key").value;
  if(context)
    url = url+"&context="+encodeURIComponent(context);
  new Ajax.Request(url,
      {method: "GET", onSuccess: function(transport){
        show_logs(transport.responseJSON, context, limit, page)}});
}

function show_logs(Ob, context, limit, page){
  if(Ob.error){
    alert("Error: "+Ob.error);
    return;
  }
  clear_results();
  var logs = Ob.logs;
  $("total_logs_field").innerHTML = Ob.total;
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
  // Make page links
  var pl = $("page_links");
  if(logs.length < Ob.total){
    /*
     * This part endeavors to display an intuitive and helpful range of page-links, while still hiding some
     *   if there are too many
     */
    var beforePages = $A($R(1,page-1));
    var totalPages = Ob.total / limit;
    // Rounding up. There _must_ be a better way to do this.
    if(Math.floor(totalPages) < totalPages)totalPages = Math.floor(totalPages)+1;
    var afterPages = $A($R(page+1,totalPages));
    var showBefore = [];
    var showAfter = [];
    // Show at least 3 pages before and after
    while(showBefore.length < 3 && beforePages.length > 0)showBefore.unshift(beforePages.pop());
    while(showAfter.length < 3 && afterPages.length > 0)showAfter.push(afterPages.shift());
    // before or after can add more if the other did not use all 3
    var borrow = 6 - showAfter.length - showBefore.length;
    while(borrow > 0 && beforePages.length > 0)
      showBefore.unshift(beforePages.pop());
    while(borrow > 0 && afterPages.length > 0)
      showAfter.push(afterPages.shift());
    // Finally, since we'll be linking to the first and last pages regardless, we don't want to hide
    // just one page, so we check here
    if(beforePages.length < 3)
      while(beforePages.length > 0)showBefore.unshift(beforePages.pop());
    if(afterPages.length < 3)
      while(afterPages.length > 0)showAfter.push(afterPages.shift());
    // Lastly insert the links
    var page_link = function(p){
      return Builder.node("a", {className: "page_link", onClick: "clicked_page("+p+")"}, ""+p);
    };
    if(beforePages.length > 0){
      pl.appendChild(page_link(1));
      pl.appendChild(Builder.node("span", "..."));
    }
    showBefore.each(function(p){
        pl.appendChild(page_link(p));
        pl.appendChild(Builder.node("span", ", "));
    });
    pl.appendChild(Builder.node("span",{style: "font-weight:bold;"}, ""+page));
    showAfter.each(function(p){
        pl.appendChild(Builder.node("span", ", "));
        pl.appendChild(page_link(p));
    });
    if(afterPages.length > 0){
      pl.appendChild(Builder.node("span", "..."));
      pl.appendChild(page_link(totalPages));
    }
  }
  else
    pl.appendChild(Builder.node("span", "1"));
}
