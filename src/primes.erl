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
  Start = erlang:timestamp(),
  spawn(fun() -> sieve(N, Pid) end),
  receive
    {done, Primes} ->
      End = erlang:timestamp(),
      TDiff = timer:now_diff(End, Start),
      io:format("Calculated ~p primes in ~p seconds:\n", [length(Primes), TDiff / 1000000]),
      io:format("~w\n", [Primes])
  end.

-spec sieve(pos_integer(), pid()) -> any().
sieve(N, Callback) ->
  % Place not_prime instead of 1 so indices match the numbers.
  sieve(Callback, N, 2, [not_prime] ++ lists:seq(2, N)).

sieve(Callback, N, Prime, Marked) ->
  NewMarked = mark_multiples_of(Prime, N, Marked),
  {_, [_ | UpcomingUnmarked]} = lists:split(Prime - 1, Marked), % Only use marked elements coming after current prime
  NextPrime = next_unmarked(UpcomingUnmarked),
  NLimit = math:sqrt(N),
  if
    NextPrime > NLimit -> % Stop early if above sqrt(N)
      respond_with_primes(Callback, NewMarked);
    true ->
      case NextPrime of
        no_unmarked -> respond_with_primes(Callback, NewMarked);
        _ -> sieve(Callback, N, NextPrime, NewMarked)
      end
  end.


mark_multiples_of(N, Limit, Marked) ->
  mark_multiples_of(N, N + N, Limit, Marked).

mark_multiples_of(_N, Index, Limit, Marked) when Index > Limit ->
  Marked;
mark_multiples_of(N, Index, Limit, Marked) ->
  {Left, [_NotPrime | Right]} = lists:split(Index - 1, Marked),
  NewMarked = Left ++ [not_prime] ++ Right,
  mark_multiples_of(N, Index + N, Limit, NewMarked).

next_unmarked([]) ->
  no_unmarked;
next_unmarked([H | T]) ->
  case H of
    not_prime -> next_unmarked(T);
    Unmarked -> Unmarked
  end.


respond_with_primes(Callback, Marked) ->
  Primes = all_unmarked(Marked),
  Callback ! {done, Primes}.

all_unmarked(Marked) ->
  First = next_unmarked(Marked),
  all_unmarked(First, Marked, [First]).

all_unmarked(Current, Marked, Unmarked) ->
  {_, [_ | UpcomingMarked]} = lists:split(Current - 1, Marked), % Only use marked elements coming after current
  Next = next_unmarked(UpcomingMarked),
  case Next of
    no_unmarked ->
      Unmarked;
    _ ->
      NewUnmarked = Unmarked ++ [Next],
      all_unmarked(Next, Marked, NewUnmarked)
  end.


