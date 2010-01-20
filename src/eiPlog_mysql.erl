-module(eiPlog_mysql).
-export([init/0,
         some_logs/5, 
         some_logs/6, 
         all_logs/3, 
         all_logs/4, 
         add_log/5, 
         new_app/1, 
         new_event/2, 
         delete_app/1, 
         delete_event/2, 
         applications/0, 
         events/1]).
-compile(export_all).

connect()->
  mysql:start_link(p1, "localhost", "rails", "railspw", "eiPlog"),
  mysql:connect(p1, "localhost", undefined, "rails", "railspw", "eiPlog", true),
  mysql:connect(p1, "localhost", undefined, "rails", "railspw", "eiPlog", true),
  mysql:connect(p1, "localhost", undefined, "rails", "railspw", "eiPlog", true),
  mysql:connect(p1, "localhost", undefined, "rails", "railspw", "eiPlog", true),
  mysql:prepare(logs_add, << "INSERT INTO logs(event_id, time, context, details) VALUES(?, now(), ?, AES_ENCRYPT(?,?))" >>),
  mysql:prepare(logs_all_by_event, << "SELECT context, time, AES_DECRYPT(details,?) FROM logs WHERE event_id = ?" >>),
  mysql:prepare(logs_all_by_context, << "SELECT time, AES_DECRYPT(details,?) FROM logs WHERE event_id = ? AND context = ?" >>),
  mysql:prepare(logs_some_by_event, << "SELECT context, time, AES_DECRYPT(details,?) FROM logs WHERE event_id = ? AND time >= ? AND time <= ?" >>),
  mysql:prepare(logs_some_by_context, << "SELECT time, AES_DECRYPT(details,?) FROM logs WHERE event_id = ? AND context = ? AND time >= ? AND time <= ?">>),
  mysql:prepare(applications, <<"SELECT id, name FROM applications WHERE deleted_at IS NULL">>),
  mysql:prepare(get_app_id, <<"SELECT id FROM applications WHERE deleted_at IS NULL AND name = ?">>),
  mysql:prepare(new_app, <<"INSERT INTO applications(name,created_at) VALUES(?,now())">>),
  mysql:prepare(delete_app, <<"UPDATE applications SET deleted_at = now() WHERE id = ?" >>),
  mysql:prepare(events, <<"SELECT id, application_id, name FROM events WHERE deleted_at IS NULL">>),
  mysql:prepare(get_event_id, <<"SELECT id FROM events WHERE deleted_at IS NULL AND application_id = ? AND name = ?">>),
  mysql:prepare(new_event, <<"INSERT INTO events(name,application_id,created_at) VALUES(?,?,now())">>),
  mysql:prepare(delete_event, <<"UPDATE events SET deleted_at = now() WHERE id = ?" >>),
  mysql:prepare(move_logs, <<"INSERT INTO deleted_logs (SELECT * FROM logs WHERE event_id = ?)">>),
  mysql:prepare(delete_logs, <<"DELETE FROM logs WHERE event_id = ?">>),
  ok.
execute(What)->execute(What,[]).
execute(What,Args)->
  case catch mysql:execute(p1,What,Args) of
    {'EXIT',{noproc,_}}->connect(),execute(What,Args);
    {updated,_}->updated;
    {data,GoodData}->mysql:get_result_rows(GoodData)
  end.

get_event_id(AppName, EventName, Fun)->
  librarian ! {event_id, AppName, EventName, self()},
  receive
    {event_id, EventId}->
      Fun(EventId);
    {error, Problem}->Problem
  end.

some_logs(AppName, EventName, Context, Before, After, Key)->
  get_event_id(AppName, EventName, fun(EventId)->
      execute(logs_some_by_context, [Key, EventId, Context, After, Before])
    end).

some_logs(AppName, EventName, Before, After, Key)->
  get_event_id(AppName, EventName, fun(EventId)->
      execute(logs_some_by_event, [Key, EventId, After, Before])
    end).

all_logs(AppName, EventName, Key)->
  get_event_id(AppName, EventName, fun(EventId)->
      execute(logs_all_by_event, [Key, EventId])
    end).

all_logs(AppName, EventName, Context, Key)->
  get_event_id(AppName, EventName, fun(EventId)->
      execute(logs_all_by_context, [Key, EventId, Context])
    end).

add_log(AppName, EventName, Context, Details, Key)->
  get_event_id(AppName, EventName, fun(EventId)->
      execute(logs_add, [EventId, Context, Details, Key]), ok
    end).

new_app(AppName)->
  librarian ! {new_app, AppName, self()},
  receive
    {new_app, ok}->ok;
    {error, Reason}->Reason
  end.

new_event(AppName, EventName)->
  librarian ! {new_event, AppName, EventName, self()},
  receive
    {new_event, ok}->ok;
    {error, Reason}->Reason
  end.

delete_app(AppName)->
  librarian ! {delete_app, AppName, self()},
  receive
    {delete_app, ok}->ok;
    {error, Reason}->Reason
  end.

delete_event(AppName, EventName)->
  librarian ! {delete_event, AppName, EventName, self()},
  receive
    {delete_event, ok}->ok;
    {error, Reason}->Reason
  end.

applications()->
  librarian ! {applications, self()},
  receive
    {applications, List}->List
  end.

events(AppName)->
  librarian ! {events, AppName, self()},
  receive
    {events, List}->List;
    {error, Reason}->Reason
  end.



% librarian is a proc that keeps data on names and ids for apps and events, as well as
%   performs all changes to those tables
init()->
  erlang:display("I AM INITING"),
  register(librarian, spawn(fun librarian/0)),
  ok.

librarian()->
  Apps = dict:from_list([{binary_to_list(Name),Id} || [Id,Name] <- execute(applications)]),
  Events = dict:from_list([{{AppId,binary_to_list(Name)},Id} || [Id, AppId, Name] <- execute(events)]),
  librarian(Apps,Events).

librarian(Apps,Events)->
  receive
    {event_id, AppName, EventName, Pid}->
      Pid ! case dict:find(AppName, Apps) of
        {ok, AppId}->
          case dict:find({AppId, EventName}, Events) of
            {ok, EventId}->
              {event_id, EventId};
            error->
              {error, bad_event}
          end;
        error->
          {error, bad_app}
      end,
      librarian(Apps,Events);
    {new_app, Name, Pid}->
      case dict:find(Name, Apps) of
        {ok, _Duplicate}->
          Pid ! {error, duplicate_name},
          librarian(Apps,Events);
        error->
          execute(new_app,[Name]),
          [[Id]] = execute(get_app_id,[Name]),
          Pid ! {new_app, ok},
          librarian(dict:store(Name,Id,Apps),Events)
      end;
    {new_event, AppName, EventName, Pid}->
      case dict:find(AppName, Apps) of
        {ok, AppId}->
          case dict:find({AppId, EventName}, Events) of
            {ok, _Duplicate}->
              Pid ! {error, duplicate_name},
              librarian(Apps,Events);
            error->
              execute(new_event, [EventName,AppId]),
              [[Id]] = execute(get_event_id,[AppId,EventName]),
              Pid ! {new_event, ok},
              librarian(Apps,dict:store({AppId,EventName},Id,Events))
          end;
        error->
          Pid ! {error, bad_app},
          librarian(Apps,Events)
      end;
    {delete_app, AppName, Pid}->
      case dict:find(AppName, Apps) of
        {ok, AppId}->
          {Relevant,Irrelevant} = lists:partition(fun({{AID,_},_}) when AID == AppId ->true;(_)->false end,dict:to_list(Events)),
          EventIds = [Id || {_,Id} <- Relevant],
          {atomic,_}=mysql:transaction(p1,fun()->
              execute(delete_app, [AppId]),
              lists:foreach(fun librarian_delete_event/1, EventIds)
            end),
          Pid ! {delete_app, ok},
          librarian(dict:erase(AppName,Apps),dict:from_list(Irrelevant));
        error->
          Pid ! {error, bad_app},
          librarian(Apps,Events)
      end;
    {delete_event, AppName, EventName, Pid}->
      case dict:find(AppName, Apps) of
        {ok, AppId}->
          case dict:find({AppId, EventName}, Events) of
            {ok, EventId}->
              {atomic,_}=mysql:transaction(p1,fun()-> librarian_delete_event(EventId) end),
              Pid ! {delete_event, ok},
              librarian(Apps, dict:erase({AppId, EventName}, Events));
            error->
              Pid ! {error, bad_event},
              librarian(Apps,Events)
          end;
        error->
          Pid ! {error, bad_app},
          librarian(Apps,Events)
      end;
    {applications, Pid}->
      Pid ! {applications, dict:fetch_keys(Apps)},
      librarian(Apps,Events);
    {events, AppName, Pid}->
      Pid ! case dict:find(AppName, Apps) of
        {ok, AppId}->
          Evs = [Event || {AID,Event} <- dict:fetch_keys(Events), AID == AppId],
          {events, Evs};
        error->
          {error, bad_app}
      end,
      librarian(Apps,Events)
  end.

librarian_delete_event(EventId)->
  execute(move_logs, [EventId]),
  execute(delete_logs, [EventId]),
  execute(delete_event, [EventId]).
  
