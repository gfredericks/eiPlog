%% @author Gary Fredericks

-module(eiPlog_mysql).
-export([init/0,
         logs/8, 
         logs/9, 
         add_log/5, 
         new_app/1, 
         new_event/2, 
         delete_app/1, 
         delete_event/2, 
         applications/0, 
         good_name/1,
         events/1]).
-compile(export_all).
-define(MAX_NAME_LENGTH, 255).
-define(MIN_NAME_LENGTH, 3).

connect()->
  {ok, [L]} = file:consult("properties"),
  Props = dict:from_list(L),
  [Host, User, Password, Database] = lists:map(fun(X)->
        dict:fetch(X, Props) 
    end, [host, user, password, database]),
  mysql:start_link(p1, Host, User, Password, Database),
  mysql:connect(p1, Host, undefined, User, Password, Database, true),
  mysql:connect(p1, Host, undefined, User, Password, Database, true),
  mysql:connect(p1, Host, undefined, User, Password, Database, true),
  mysql:connect(p1, Host, undefined, User, Password, Database, true),
  mysql:prepare(logs_by_event_asc, << "SELECT context, time, AES_DECRYPT(details,?) FROM logs USE INDEX(without_context) WHERE event_id = ? AND time >= ? AND time <= ? ORDER BY time ASC LIMIT ?, ?" >>),
  mysql:prepare(logs_by_context_asc, << "SELECT time, AES_DECRYPT(details,?) FROM logs USE INDEX(with_context) WHERE event_id = ? AND context = ? AND time >= ? AND time <= ? ORDER BY time ASC LIMIT ?, ?">>),
  mysql:prepare(logs_by_event_desc, << "SELECT context, time, AES_DECRYPT(details,?) FROM logs USE INDEX(without_context) WHERE event_id = ? AND time >= ? AND time <= ? ORDER BY time DESC LIMIT ?, ?" >>),
  mysql:prepare(logs_by_context_desc, << "SELECT time, AES_DECRYPT(details,?) FROM logs USE INDEX(with_context) WHERE event_id = ? AND context = ? AND time >= ? AND time <= ? ORDER BY time DESC LIMIT ?, ?">>),
  mysql:prepare(count_by_event, << "SELECT count(*) FROM logs USE INDEX(without_context) WHERE event_id = ? AND time >= ? AND time <= ?" >>),
  mysql:prepare(count_by_context, << "SELECT count(*) FROM logs USE INDEX(with_context) WHERE event_id = ? AND context = ? AND time >= ? AND time <= ?">>),
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
call_proc(What)->call_proc(What,[]).
call_proc(What,Args)->
  QB = list_to_binary(lists:flatten(io_lib:format("CALL ~w(~s)", [What, string:join([mysql:encode(X) || X<-Args], ", ")]))),
  case catch mysql:fetch(p1, QB) of
    {'EXIT',{Reason,_}}->
      io:format("Reconnecting from reason = ~w~n", [Reason]),
      connect(),
      execute(What,Args);
    {updated,_}->updated;
    {data,GoodData}->mysql:get_result_rows(GoodData)
  end.

execute(What)->execute(What,[]).
execute(What,Args)->
  case catch mysql:execute(p1,What,Args) of
    {'EXIT',{Reason,_}}->
      io:format("Reconnecting from reason = ~w~n", [Reason]),
      connect(),
      execute(What,Args);
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

logs(AppName, EventName, Context, Before, After, Key, Order, Limit, Page)->
  {Offset,RowCount} = limit_page(Limit,Page),
  get_event_id(AppName, EventName, fun(EventId)->
      Logs = case RowCount of 0->[];
              _Positive->execute(case Order of 'ASC'->logs_by_context_asc; 'DESC'->logs_by_context_desc end, [Key, EventId, Context, After, Before, Offset, RowCount])
        end,
      [[Count]] = execute(count_by_context, [EventId, Context, After, Before]),
      {Logs, Count}
    end).

logs(AppName, EventName, Before, After, Key, Order, Limit, Page)->
  {Offset,RowCount} = limit_page(Limit,Page),
  get_event_id(AppName, EventName, fun(EventId)->
      Logs = case RowCount of 0->[];
              _Positive->execute(case Order of 'ASC'->logs_by_event_asc; 'DESC'->logs_by_event_desc end, [Key, EventId, After, Before, Offset, RowCount])
        end,
      [[Count]] = execute(count_by_event, [EventId, After, Before]),
      {Logs, Count}
    end).

limit_page(Limit, Page)->
  case Limit of
    undefined->{0, 10000000000000000} ;
    A when is_integer(A), A > 0 -> 
      P = case Page of B when is_integer(B), B > 0 ->B; _Else->1 end,
      {(P-1)*Limit, Limit};
    A when is_integer(A), A == 0 ->{0,0}
  end.

add_log(AppName, EventName, Context, Details, Key)->
  get_event_id(AppName, EventName, fun(EventId)->
      call_proc(logs_add, [EventId, Context, Details, Key]), ok
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
      librarian_event_id(Apps, Events, AppName, EventName, Pid);
    {new_app, Name, Pid}->
      librarian_new_app(Apps, Events, Name, Pid);
    {new_event, AppName, EventName, Pid}->
      librarian_new_event(Apps, Events, AppName, EventName, Pid);
    {delete_app, AppName, Pid}->
      librarian_delete_app(Apps, Events, AppName, Pid);
    {delete_event, AppName, EventName, Pid}->
      librarian_delete_event(Apps, Events, AppName, EventName, Pid);
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

librarian_event_id(Apps, Events, AppName, EventName, Pid)->
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
  librarian(Apps,Events).

librarian_new_app(Apps, Events, Name, Pid)->
  case dict:find(Name, Apps) of
    {ok, _Duplicate}->
      librarian_error(Apps, Events, Pid, duplicate_name);
    error->
      case good_name(Name) of
        false->
          librarian_error(Apps, Events, Pid, bad_name);
        true->
          execute(new_app,[Name]),
          [[Id]] = execute(get_app_id,[Name]),
          Pid ! {new_app, ok},
          librarian(dict:store(Name,Id,Apps),Events)
      end
  end.

librarian_new_event(Apps, Events, AppName, EventName, Pid)->
  case dict:find(AppName, Apps) of
    {ok, AppId}->
      case dict:find({AppId, EventName}, Events) of
        {ok, _Duplicate}->
          librarian_error(Apps, Events, Pid, duplicate_name);
        error->
          case good_name(EventName) of
            false->
              librarian_error(Apps, Events, Pid, bad_name);
            true->
              updated = execute(new_event, [EventName,AppId]),
              %[[Id]] = execute(get_event_id,[AppId,EventName]),
              case execute(get_event_id,[AppId, EventName]) of
                [[Id]]->
                  Pid ! {new_event, ok},
                  librarian(Apps,dict:store({AppId,EventName},Id,Events));
                []-> io:format("WTF!!!! Cannot create ev name = ~s~n", [EventName])
              end
          end
      end;
    error->
      librarian_error(Apps, Events, Pid, bad_app)
  end.

librarian_delete_app(Apps, Events, AppName, Pid)->
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
      librarian_error(Apps, Events, Pid, bad_app)
  end.

librarian_delete_event(Apps, Events, AppName, EventName, Pid)->
  case dict:find(AppName, Apps) of
    {ok, AppId}->
      case dict:find({AppId, EventName}, Events) of
        {ok, EventId}->
          {atomic,_}=mysql:transaction(p1,fun()-> librarian_delete_event(EventId) end),
          Pid ! {delete_event, ok},
          librarian(Apps, dict:erase({AppId, EventName}, Events));
        error->
          librarian_error(Apps, Events, Pid, bad_event)
      end;
    error->
      librarian_error(Apps, Events, Pid, bad_app)
  end.



librarian_error(Apps, Events, Pid, Error)->
  Pid ! {error, Error},
  librarian(Apps, Events).

librarian_delete_event(EventId)->
  execute(move_logs, [EventId]),
  execute(delete_logs, [EventId]),
  execute(delete_event, [EventId]).

good_name(S)->
  case re:run(S, lists:flatten(io_lib:format("^[-0-9a-zA-Z_]{~b,~b}$", [?MIN_NAME_LENGTH, ?MAX_NAME_LENGTH]))) of
    nomatch->false;
    _Somematch->true
  end.
