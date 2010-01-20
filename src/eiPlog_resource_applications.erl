%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(eiPlog_resource_applications).
-export([init/1, 
         resource_exists/2,
         is_conflict/2,
         delete_resource/2,
         create_path/2,
         content_types_accepted/2,
         allowed_methods/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_accepted(ReqData, State)->
  {[{"text/plain", create_path}, {"text/html",create_path}], ReqData, State}.

resource_exists(ReqData, State)->
  Res = lists:member(dict:fetch(app_name, wrq:path_info(ReqData)), eiPlog_mysql:applications()),
  {Res, ReqData, State}.

is_conflict(ReqData, State)->
  resource_exists(ReqData, State).

allowed_methods(ReqData, Context)->
  {['PUT', 'DELETE'], ReqData, Context}.

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
