%
% This only exists because I can't figure out any good way of storing
%   an appropriately global variable in erlang.
%

-module(eiPlog_keyholder).
-export([start/1, get_key/0]).

start(Key)->
  true=register(keyholder, spawn(fun()->daemon(Key) end)),
  ok.

get_key()->
  case get(eiPlog_database_key) of
    undefined->
      keyholder ! {get_key, self()},
      K = receive {key, Key} -> Key end,
      put(eiPlog_database_key, K),
      K;
    Defined->Defined
  end.

daemon(Key)->
  receive
    {get_key, Pid}->
      Pid ! {key, Key},
      daemon(Key)
  end.
