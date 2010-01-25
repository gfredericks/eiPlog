%% @author Gary Fredericks
%% @doc Example webmachine_resource.

-module(eiPlog_resource_applications).
-export([init/1, 
         resource_exists/2,
         is_conflict/2,
         delete_resource/2,
         content_types_provided/2,
         to_json/2,
         create_path/2,
         content_types_accepted/2,
         allowed_methods/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined};
init([all]) -> {ok, all}.

content_types_accepted(ReqData, State)->
  {[{"text/plain", create_path}, {"text/html",create_path}], ReqData, State}.

resource_exists(ReqData, undefined)->
  Res = lists:member(dict:fetch(app_name, wrq:path_info(ReqData)), eiPlog_mysql:applications()),
  {Res, ReqData, undefined};

resource_exists(ReqData, all)->{true, ReqData, all}.

is_conflict(ReqData, State)->
  resource_exists(ReqData, State).

allowed_methods(ReqData, undefined)->
  {['PUT', 'DELETE'], ReqData, undefined};

allowed_methods(ReqData, all)->{['GET'], ReqData, all}.

content_types_provided(ReqData, Context)->
  {[{"application/json", to_json}, 
    {"text/html", to_json},
    {"text/plain", to_json}], ReqData, Context}.

delete_resource(ReqData, Context)->
  Res = case eiPlog_mysql:delete_app(dict:fetch(app_name, wrq:path_info(ReqData))) of
    ok->true;
    bad_app->false
  end,
  {Res, ReqData, Context}.

create_path(ReqData, Context)->
  Name = dict:fetch(app_name, wrq:path_info(ReqData)),
  eiPlog_mysql:new_app(Name),
  {Name, ReqData, Context}.

to_json(ReqData, all) ->
  Res=rfc4627:encode(lists:map(fun list_to_binary/1, eiPlog_mysql:applications())),
  {Res, ReqData, all}.
