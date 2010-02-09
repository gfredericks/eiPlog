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
  Options = [X || {_, V} = X <-
    lists:map(fun(Str)->{list_to_atom(Str),wrq:get_qs_value(Str,R)} end, 
      ["before", "after", "context", "order", "limit", "page_key"]),
    V/=undefined],
  Options2 = lists:map(fun({limit, L})->{limit, list_to_integer(L)};
                          ({order, "DESC"})->{order, 'DESC'};
                          ({order, "ASC"})->{order, 'ASC'};
                          ({before, Date})->{before, parse_date(Date)};
                          ({'after', Date})->{'after', parse_date(Date)};
                          (X)->X end, Options),

  {Logs, Count, Prev, Next} = eiPlog_mysql:logs(AppName, EventName, wrq:get_qs_value("key", R), Options2),
  Ob = {obj, [X || {_, V} = X <- [{"total", Count},
                                  {"next_page", Next},
                                  {"prev_page", Prev},
                                  {"logs", Logs}], V/=undefined]},
 
  % If any details came in as undefined, or if JSON throws an error, likely
  %   there was a bad key given
  Res = case lists:any(fun({obj, KVs})->lists:any(fun({"details", undefined})->true;(_)->false end, KVs) end, Logs) of
    true -> ?BAD_KEY_JSON;
    false->
      try rfc4627:encode(Ob) of
        JSON->JSON
        catch exit:AnyError -> rfc4627:encode({obj, [{"error", lists:flatten(io_lib:format("~w", [AnyError]))}]})
      end
  end,
  {Res, R, S}.

content_types_provided(ReqData, Context)->
  {[{"application/json", to_json}, 
    {"text/html", to_json},
    {"text/plain", to_json}], ReqData, Context}.

parse_date([A,B,C,D,E,F,G,H,I,J,K,L,M,N])->
  {datetime, {{list_to_integer([A,B,C,D]),
               list_to_integer([E,F]),
               list_to_integer([G,H])},
              {list_to_integer([I,J]),
               list_to_integer([K,L]),
               list_to_integer([M,N])}}}.
