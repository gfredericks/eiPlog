%% @author Gary Fredericks

%% @doc TEMPLATE.

-module(eiPlog).
-author('author <author@example.com>').
-export([start/0, start/1, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    eiPlog_deps:ensure(),
    ensure_started(crypto),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    eiPlog_sup:start_link().

%% @spec start() -> ok
%% @doc Start the eiPlog server.
start()->
  % Key missing
  io:format("Missing encryption key argument~n"),
  halt().

start([Key]) ->
    eiPlog_deps:ensure(),
    ensure_started(crypto),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    eiPlog_keyholder:start(atom_to_list(Key)),
    application:start(eiPlog).

%% @spec stop() -> ok
%% @doc Stop the eiPlog server.
stop() ->
    Res = application:stop(eiPlog),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
