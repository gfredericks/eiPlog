%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(eiPlog_resource_events).
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
  AppName = wrq:path_info(app_name, ReqData),
  EventName = wrq:path_info(event_name, ReqData),
  Res = case eiPlog_mysql:events(AppName) of
    bad_app->false;
    Events->lists:member(EventName, Events)
  end,
  {Res, ReqData, State}.

is_conflict(ReqData, State)->
  AppName = wrq:path_info(app_name, ReqData),
  resource_exists(ReqData, State) orelse (not lists:member(AppName, eiPlog_mysql:applications())).

allowed_methods(ReqData, Context)->
  {['PUT', 'DELETE'], ReqData, Context}.

delete_resource(ReqData, Context)->
  Res = case eiPlog_mysql:delete_event(wrq:path_info(app_name, ReqData), wrq:path_info(event_name, ReqData)) of
    ok->true;
    bad_app->false
  end,
  {Res, ReqData, Context}.

create_path(ReqData, Context)->
  AppName = wrq:path_info(app_name, ReqData),
  EventName = wrq:path_info(event_name, ReqData),
  eiPlog_mysql:new_event(AppName, EventName),
  {AppName ++ "/" ++ EventName, ReqData, Context}.
