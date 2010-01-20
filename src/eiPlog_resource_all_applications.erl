%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(eiPlog_resource_all_applications).
-export([init/1, 
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, Context)->
  {[{"application/json", to_json}], ReqData, Context}.

allowed_methods(ReqData, Context)->
  {['GET'], ReqData, Context}.

to_json(ReqData, State) ->
  Res=rfc4627:encode(lists:map(fun list_to_binary/1, eiPlog_mysql:applications())),
  {Res, ReqData, State}.
