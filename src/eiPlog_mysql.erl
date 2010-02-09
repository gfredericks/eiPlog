%% @author Gary Fredericks

-module(eiPlog_mysql).
-export([init/0,
         logs/4,
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
-define(MYSQL_INFINITY, 10000000000000000).
-define(BASE_QUERY_WITH_CONTEXT, "SELECT id, time, AES_DECRYPT(details, ~s) FROM logs USE INDEX(with_context) WHERE event_id = ~s AND context = ~s").
-define(BASE_QUERY_WITHOUT_CONTEXT, "SELECT id, time, context, AES_DECRYPT(details, ~s) FROM logs USE INDEX(without_context) WHERE event_id = ~s").
-record(get_params, {before, 'after', context, order, limit, page_key}).

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
  mysql:prepare(logs_add, << "INSERT INTO logs(event_id, time, context, details) VALUES(?, now(), ?, AES_ENCRYPT(?,?))" >>),
  mysql:prepare(get_time, << "SELECT time FROM logs WHERE id = ?" >>),
  mysql:prepare(count_by_event, << "SELECT count(*) FROM logs USE INDEX(without_context) WHERE event_id = ? AND time BETWEEN ? AND ?" >>),
  mysql:prepare(count_by_context, << "SELECT count(*) FROM logs USE INDEX(with_context) WHERE event_id = ? AND context = ? AND time BETWEEN ? AND ?">>),
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

% Valid options: 
%   {context, 'string()'}
%   {before, 'datetime'}
%   {after, 'datetime'}
%   {order, 'ASC' | 'DESC'}
%   {limit, 'integer()'}
%   {page_key, 'string()'}
%   

logs(AppName, EventName, Key, Options)->
  get_event_id(AppName, EventName,
    fun(EventId)->
        % Sorting ensures that 'before' and 'after' come before 'page_key'
        logs1(EventId, Key, logs_options(lists:sort(Options)))
    end).

logs_options(Opts)->
  lists:foldl(fun logs_options/2, #get_params{order='ASC'}, Opts).

logs_options({'after', {datetime,{{_,_,_},{_,_,_}}}=A}, P) -> P#get_params{'after'=A};
logs_options({before, {datetime,{{_,_,_},{_,_,_}}}=B}, P) -> P#get_params{before=B};
logs_options({context, CTX}, P) when is_list(CTX); is_binary(CTX)->
  P#get_params{context=CTX};
logs_options({limit, L}, P) when is_integer(L),  L >= 0 -> P#get_params{limit=L};
logs_options({order, O}, P) when O == 'DESC'; O == 'ASC' -> P#get_params{order=O};
logs_options({page_key, [C|_]=PK}, P) when C==$n; C==$p ->
  P#get_params{page_key=PK}.

logs1(EID, Key, Params)->
  {Data, Prev, Next} = logs_data(EID, Key, Params),
  Count = logs_count(EID, Params),
  Logs = lists:map(fun([Time, Context, Details])->{obj, [{"time", date_to_string(Time)}, {"context", Context}, {"details", Details}]};
      ([Time, Details])->{obj, [{"time", date_to_string(Time)}, {"details", Details}]} end, Data),
  {Logs, Count, Prev, Next}.

logs_data(EID, Key, Params)->
  {Op,Rev} = (fun
      (_,undefined)->{undefined, false};
      ('ASC', [$n|_])->{">", false};
      ('ASC', [$p|_])->{"<", true};
      ('DESC', [$n|_])->{"<", false};
      ('DESC', [$p|_])->{">", true}
    end)(Params#get_params.order, Params#get_params.page_key),

  Query = 
    % Initial query, with or without context
    lists:flatten(case Params#get_params.context of
      undefined->
        io_lib:format(?BASE_QUERY_WITHOUT_CONTEXT, [mysql:encode(X)||X<-[Key, EID]]);
      CTX->
        io_lib:format(?BASE_QUERY_WITH_CONTEXT, [mysql:encode(X)||X<-[Key,EID,CTX]])
    end) ++ 

    % Before and After
    case Params#get_params.'after' of
      undefined->"";
      SomeTime->
        case Op of ">"->"";_->" AND time >= " ++ mysql:encode(SomeTime) end
    end ++
    case Params#get_params.before of
      undefined->"";
      OtherTime->
        case Op of "<"->"";_->" AND time <= " ++ mysql:encode(OtherTime) end
    end ++

    case Params#get_params.page_key of
      undefined->"";
      [_|PK]->
        Id = list_to_integer(PK),
        [[Time]] = execute(get_time, [Id]),
        " AND (time " ++ Op ++ " " ++ mysql:encode(Time) ++ " OR (time = " ++
          mysql:encode(Time) ++ " AND id " ++ Op ++ " " ++ mysql:encode(Id) ++ "))"
    end ++


    case Params#get_params.order of
      undefined->"";
      O->
        O1 = case {O,Rev} of {'ASC',true}->'DESC';
                             {'DESC', true}->'ASC';
                             {_,false}->O end,
        Ord = atom_to_list(O1),
        " ORDER BY time " ++ Ord ++ ", id " ++ Ord
    end ++
    case Params#get_params.limit of
      undefined->"";
      L->" LIMIT " ++ integer_to_list(L+1) % Pull extra record to check for more
    end,
  D = mysql:get_result_rows(element(2,mysql:fetch(p1, Query))),
  {DD, Extra} = case Params#get_params.limit of undefined->{D, undefined}; 
    LL->
      case (catch lists:split(LL, D)) of
        {'EXIT',_}->{D,[]};
        Worked->Worked
      end
  end,
  Data = case Rev of true->lists:reverse(DD);false->DD end,
  {Next, Prev} = 
    case Params#get_params.limit of
      undefined->{undefined, undefined};
      _Elze->
        NN = list_to_binary([$n|integer_to_list(hd(lists:last(Data)))]),
        PP = list_to_binary([$p|integer_to_list(hd(hd(Data)))]),
        case Rev of
          false->
            {case Extra of []->undefined;_Else->NN end,
              case Params#get_params.page_key of undefined->undefined; _Else->PP end};
          true->
            {NN, case Extra of []->undefined;_else->PP end}
        end
    end,
  {[tl(Rec) || Rec <- Data], Prev, Next}.

logs_count(EID, #get_params{before=undefined}=P)->
  logs_count(EID, P#get_params{before={datetime, {{3005,1,1},{1,1,1}}}});

logs_count(EID, #get_params{'after'=undefined}=P)->
  logs_count(EID, P#get_params{'after'={datetime, {{1995,1,1},{1,1,1}}}});

logs_count(EID, #get_params{'after'=A, before=B, context=undefined})->
  [[C]]=execute(count_by_event, [EID, A, B]),
  C;
logs_count(EID, #get_params{'after'=A, before=B, context=CTX})->
  [[C]]=execute(count_by_context, [EID, CTX, A, B]),
  C.



add_log(AppName, EventName, Context, Details, Key)->
  get_event_id(AppName, EventName, fun(EventId)->
      call_proc(logs_add, [EventId, Context, Details, Key])
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

date_to_string({datetime, {{Year, Month, Day}, {Hour, Minute, Second}}})->
  list_to_binary(lists:flatten(io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b",[Year, Month, Day, Hour, Minute, Second]))).

