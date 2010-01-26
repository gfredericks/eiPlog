package eip.eiPlog;

import eip.eiPlog.ClojureAPI;

public class Event {

  private String name;
  private Application app;

  public void log(String context, String details){
    ClojureAPI.log(this.app.getName(), this.name, context, details);
  }

  // This should have package-level scope only; not for public use

  Event(Application a, String n){
    this.name = n;
    this.app = a;
  }

  public String getName(){
    return name;
  }
}
