package eip.eiPlog;

import eip.eiPlog.ClojureAPI;

public class Application {

  private String name;

  public static void main(String[] args){
    List<Application> apps = getAll();
    for(Application app: apps){
      System.out.println("An app is "+app.getName());
    }
  }
  public static List<Application> getAll(){
    List<String> apps = ClojureAPI.applications();
    List<Application> ret = new List<Application>();
    for(Application app: apps){
      ret.add(new Application(app));
    }
    return ret;
  }

  private Application(String name){
    this.name = name;
  }

  public String getName(){
    return name;
  }
}
