%%%-------------------------------------------------------------------
%%% @author marti
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
  Pid = self(),
  spawn(fun() -> sieve(N, Pid) end),
  receive
    {done, Primes} -> Primes
  end.

sieve(N, Callback) ->
  sieve(Callback, lists:seq(2, N), []).

sieve(Callback, [], Primes) ->
  Callback ! {done, Primes};
sieve(Callback, [N | Candidates], Primes) ->
  io:format("checking ~p out of ~p candidates, up to ~p primes ~p\n", [N, length(Candidates), length(Primes), Primes]),
  NewPrimes = Primes ++ [N],
  NewCandidates = [P || P <- Candidates, P rem N =/= 0],
  sieve(Callback, NewCandidates, NewPrimes).
