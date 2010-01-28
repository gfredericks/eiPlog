%% @author Gary Fredericks
%% @doc Example webmachine_resource.

-module(eiPlog_resource_applications).
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

init([]) -> {ok, undefined};
init([all]) -> {ok, all}.

content_types_accepted(ReqData, State)->
  {[{"text/plain", from_text}, {"text/html", from_text}], ReqData, State}.

resource_exists(ReqData, undefined)->
  Res = lists:member(wrq:path_info(app_name, ReqData), eiPlog_mysql:applications()),
  {Res, ReqData, undefined};

resource_exists(ReqData, all)->{true, ReqData, all}.

is_conflict(ReqData, State)->
  resource_exists(ReqData, State).

post_is_create(R, S)-> {true, R, S}.

allow_missing_post(R, S)->{true, R, S}.

allowed_methods(ReqData, undefined)->
  {['PUT', 'DELETE', 'POST'], ReqData, undefined};

allowed_methods(ReqData, all)->{['GET'], ReqData, all}.

content_types_provided(ReqData, Context)->
  {[{"application/json", to_json}, 
    {"text/html", to_json},
    {"text/plain", to_json}], ReqData, Context}.

delete_resource(ReqData, Context)->
  Res = case eiPlog_mysql:delete_app(wrq:path_info(app_name, ReqData)) of
    ok->true;
    bad_app->false
  end,
  {Res, ReqData, Context}.

create_path(ReqData, Context)->
  Name = wrq:path_info(app_name, ReqData),
  {Name, ReqData, Context}.

from_text(ReqData, Context)->
  Name = wrq:path_info(app_name, ReqData),
  ok = eiPlog_mysql:new_app(Name),
  {true, ReqData, Context}.

to_json(ReqData, all) ->
  Res=rfc4627:encode(lists:map(fun list_to_binary/1, eiPlog_mysql:applications())),
  {Res, ReqData, all}.
