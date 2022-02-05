%%%-------------------------------------------------------------------
%%% @author marti
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. feb. 2022 14:05
%%%-------------------------------------------------------------------
-module(primes).
-author("marti").

%% API
-export([calculate/1]).

calculate(N) ->
  spawn(fun() -> sieve(N, self()) end),
  receive
    {done, Primes} -> Primes
  end.

sieve(N, Callback) when N =< 1 ->
  io:format("Can't calculate primes up to ~p\n", N);
sieve(2, Callback) ->
  {};
sieve(N, Callback) ->
  {}.
