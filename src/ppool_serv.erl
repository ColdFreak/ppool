-module(ppool_serv).
-behaviour(gen_server).
-export([start/4]).
-export([init/1]).

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

