package eip.eiPlog;

import java.util.List;
import java.util.ArrayList;
import eip.eiPlog.ClojureAPI;

public class Application {

  private String name;

  public static class LogFast implements Runnable {
    private String c,d;
    private Event e;
    public LogFast(String c,String d,Event e){
      this.c=c;
      this.d=d;
      this.e=e;
    }
    public void run(){
      e.log(c,d);
    }
  }

  public static void main(String[] args){
    String appName = args[0];
    String eventName = args[1];
    String context = args[2];
    String details = args[3];
    Event ev = Application.find(appName).findEvent(eventName);
    Runnable[] josh = new Runnable[50];
    Thread[] ts = new Thread[50];
    for(int i=0;i<50;i++)
      josh[i]=new LogFast(context, details, ev);
    for(int i=0;i<50;i++)
      ts[i]=new Thread(josh[i]);
    for(int i=0;i<50;i++){
      try{
        ts[i].join();
      }
      catch(InterruptedException e) {
        System.out.println("Ok");
      }
    }
  }
  public static List<Application> getAll(){
    List apps = ClojureAPI.applications();
    List<Application> ret = new ArrayList<Application>();
    for(Object app: apps){
      ret.add(new Application((String)app));
    }
    return ret;
  }

  public static Application find(String name) throws NoSuchThingException{
    List<Application> apps = getAll();
    for(Application app : apps){
      if(name.equals(app.getName())) return app;
    }
    throw new NoSuchThingException("No application with name \""+name+"\"");
  }

  public Event findEvent(String name) throws NoSuchThingException{
    List<Event> evs = getEvents();
    for(Event ev : evs){
      if(name.equals(ev.getName()))return ev;
    }
    throw new NoSuchThingException("No event in application \""+this.name+"\" with name \""+name+"\"");
  }

  public List<Event> getEvents(){
    List events = ClojureAPI.events(this.name);
    List<Event> ret = new ArrayList<Event>();
    for(Object ev: events){
      ret.add(new Event(this, (String)ev));
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
