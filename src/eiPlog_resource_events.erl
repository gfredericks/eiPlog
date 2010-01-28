%% @author Gary Fredericks
%% @doc Example webmachine_resource.

-module(eiPlog_resource_events).
-export([init/1, 
         resource_exists/2,
         is_conflict/2,
         delete_resource/2,
         content_types_provided/2,
         to_json/2,
         post_is_create/2,
         allow_missing_post/2,
         create_path/2,
         from_text/2,
         content_types_accepted/2,
         allowed_methods/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {{trace, "/tmp"}, undefined};
init([all]) -> {ok, all}.

content_types_accepted(ReqData, undefined)->
  {[{"text/plain", from_text}, {"text/html", from_text}], ReqData, undefined}.

resource_exists(ReqData, undefined)->
  AppName = wrq:path_info(app_name, ReqData),
  EventName = wrq:path_info(event_name, ReqData),
  Res = case eiPlog_mysql:events(AppName) of
    bad_app->false;
    Events->lists:member(EventName, Events)
  end,
  {Res, ReqData, undefined};

resource_exists(ReqData, all)->
  AppName = wrq:path_info(app_name, ReqData),
  Res = lists:member(AppName, eiPlog_mysql:applications()),
  {Res, ReqData, all}.

is_conflict(ReqData, State)->
  resource_exists(ReqData, State).
  %AppName = wrq:path_info(app_name, ReqData),
  %Res = resource_exists(ReqData, State) orelse 
  %      (not lists:member(AppName, eiPlog_mysql:applications())),
  %{Res, ReqData, State}.

post_is_create(R, S)-> {true, R, S}.

allow_missing_post(R, S)->{true, R, S}.

allowed_methods(ReqData, undefined)->
  {['PUT', 'DELETE', 'POST'], ReqData, undefined};
allowed_methods(ReqData, all)->
  {['GET'], ReqData, all}.

delete_resource(ReqData, Context)->
  Res = case eiPlog_mysql:delete_event(wrq:path_info(app_name, ReqData), wrq:path_info(event_name, ReqData)) of
    ok->true;
    bad_app->false
  end,
  {Res, ReqData, Context}.

from_text(ReqData, Context)->
  AppName = wrq:path_info(app_name, ReqData),
  EventName = wrq:path_info(event_name, ReqData),
  ok = eiPlog_mysql:new_event(AppName, EventName),
  {true, ReqData, Context}.

create_path(ReqData, Context)->
  EventName = wrq:path_info(event_name, ReqData),
  {EventName, ReqData, Context}.

to_json(ReqData, State) ->
  AppName = wrq:path_info(app_name, ReqData),
  Res=rfc4627:encode(lists:map(fun list_to_binary/1, eiPlog_mysql:events(AppName))),
  {Res, ReqData, State}.

content_types_provided(ReqData, Context)->
  {[{"application/json", to_json}, 
    {"text/html", to_json},
    {"text/plain", to_json}], ReqData, Context}.
