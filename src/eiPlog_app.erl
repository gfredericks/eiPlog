%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the eiPlog application.

-module(eiPlog_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for eiPlog.
start(_Type, _StartArgs) ->
    eiPlog_deps:ensure(),
    eiPlog_mysql:init(),
    eiPlog_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for eiPlog.
stop(_State) ->
    ok.
