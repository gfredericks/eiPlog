%% @author Gary Fredericks
%% @doc Example webmachine_resource.

-module(eiPlog_resource_logs).
-export([init/1, 
         resource_exists/2,
         content_types_accepted/2,
         content_types_provided/2,
         allowed_methods/2,
         process_post/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-define(BAD_KEY_JSON, "{\"error\": \"bad-key\"}").

init([]) -> {ok, undefined}.

allowed_methods(R,S)->
  {['POST','GET'],R,S}.

resource_exists(R,S)->
  Res = case eiPlog_mysql:events(wrq:path_info(app_name, R)) of 
    bad_app->false;
    Events->
      lists:member(wrq:path_info(event_name, R), Events)
  end,
  {Res, R, S}.

content_types_accepted(R,S)->
  {[{"application/json", process_post}], R, S}.

process_post(R,S)->
  AppName = wrq:path_info(app_name, R),
  EventName = wrq:path_info(event_name, R),
  {ok, {obj, Ob}, _} = rfc4627:decode(wrq:req_body(R)),
  [{"context", CBinary}, {"details", DBinary}]=lists:sort(Ob),
  Context = binary_to_list(CBinary),
  Details = binary_to_list(DBinary),
  Res=eiPlog_mysql:add_log(AppName, EventName, Context, Details, eiPlog_keyholder:get_key()),
  {Res == updated, R, S}.

to_json(R, S) ->
  AppName = wrq:path_info(app_name, R),
  EventName = wrq:path_info(event_name, R),
  [Bef, Aft, Key, Context, Ord, Lim, PK] = 
    lists:map(fun(Str)->wrq:get_qs_value(Str,R) end, 
      ["before", "after", "key", "context", "order", "limit", "page_key"]),
  Before = case Bef of
    undefined->
      {datetime, {{3010,1,1}, {1,1,1}}};
    String1->parse_date(String1)
  end,
  After = case Aft of
    undefined->
      {datetime, {{1995,1,1}, {1,1,1}}};
    String2->parse_date(String2)
  end,
  Order = case Ord of
    "ASC"->'ASC';
    "DESC"->'DESC';
    _Else->'ASC'
  end,
  [Limit, PageKey] = lists:map(fun(undefined)->undefined; (Str)->list_to_integer(Str) end, [Lim, PK]),
  {C, L, NP} = case Context of
    undefined->
      {Logs, Count, NextPage} = eiPlog_mysql:logs(AppName, EventName, Before, After, Key, Order, Limit, PageKey),
      JSLogs = lists:map(fun([Con,Tim,Det])->
            {obj, [{"time", date_to_string(Tim)},
                   {"context", Con},
                   {"details", Det}]}
          end, Logs),
      {Count, JSLogs, NextPage};
    Context->
      {Logs, Count, NextPage} = eiPlog_mysql:logs(AppName, EventName, Context, Before, After, Key, Order, Limit, PageKey),
      JSLogs = lists:map(fun([Tim,Det])->
            {obj, [{"time", date_to_string(Tim)},
                   {"details", Det}]}
          end, Logs),
      {Count, JSLogs, NextPage}
  end,
  
  Res = case lists:any(fun({obj, [_,_,{"details", undefined}]})->true;(_)->false end, L) of
    true -> ?BAD_KEY_JSON;
    false->
      NextPage1 = case NP of undefined->[]; Defined->[{"next_page", Defined}] end,
      try rfc4627:encode({obj, [{"total", C}, {"logs", L}] ++ NextPage1}) of
        JSON->JSON
        catch exit:AnyError -> rfc4627:encode({obj, [{"error", lists:flatten(io_lib:format("~w", [AnyError]))}]})
      end
  end,
  {Res, R, S}.

content_types_provided(ReqData, Context)->
  {[{"application/json", to_json}, 
    {"text/html", to_json},
    {"text/plain", to_json}], ReqData, Context}.

date_to_string({datetime, {{Year, Month, Day}, {Hour, Minute, Second}}})->
  list_to_binary(lists:flatten(io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b",[Year, Month, Day, Hour, Minute, Second]))).

parse_date([A,B,C,D,E,F,G,H,I,J,K,L,M,N])->
  {datetime, {{list_to_integer([A,B,C,D]),
               list_to_integer([E,F]),
               list_to_integer([G,H])},
              {list_to_integer([I,J]),
               list_to_integer([K,L]),
               list_to_integer([M,N])}}}.
