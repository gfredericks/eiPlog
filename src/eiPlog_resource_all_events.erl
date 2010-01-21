%% @author Gary Fredericks
%% @doc Example webmachine_resource.

-module(eiPlog_resource_all_events).
-export([init/1, 
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, Context)->
  {[{"application/json", to_json}], ReqData, Context}.

allowed_methods(ReqData, Context)->
  {['GET'], ReqData, Context}.

resource_exists(ReqData, Context)->
  AppName = wrq:path_info(app_name, ReqData),
  Res = lists:member(AppName, eiPlog_mysql:applications()),
  {Res, ReqData, Context}.

to_json(ReqData, State) ->
  AppName = wrq:path_info(app_name, ReqData),
  Res=rfc4627:encode(lists:map(fun list_to_binary/1, eiPlog_mysql:events(AppName))),
  {Res, ReqData, State}.
