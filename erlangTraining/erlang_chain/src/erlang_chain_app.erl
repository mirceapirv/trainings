%%%-------------------------------------------------------------------
%% @doc erlang_chain public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_chain_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_chain_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
