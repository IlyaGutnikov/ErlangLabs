-module(fib).
-export([fib_p/1, fib_g/1, tail_fib/1]).
-export([test_fib_p/1, test_tail_fib/0]).

%% non tail
fib_p(0) -> 0;
fib_p(1) -> 1;
fib_p(N) -> fib_p(N-1) + fib_p(N-2).

%% with guard (when) 
fib_g(N) when N == 0 -> 0;
fib_g(N) when N == 1 -> 1;
fib_g(N) -> fib_g(N-1) + fib_g(N-2).

%% with tail
tail_fib(N) -> fib_t(N, 0, []).
fib_t(1, _, _) -> 1;
fib_t(2, _, _) -> 1;
fib_t(EndCond, 0, []) -> fib_t(EndCond, 1, [1]);
fib_t(EndCond, 1, [1]) -> fib_t(EndCond, 2, [1, 1]);
fib_t(EndCond, Counter, [X,Y|Tail]) when EndCond =/= Counter -> fib_t(EndCond, Counter + 1, [X + Y, X, Y|Tail]);
fib_t(N, N, [Head| _]) -> Head.

test_fib_p(M) -> timer:tc(fib,fib_p, [M]).

test_tail_fib() -> timer:tc(fib,tail_fib, [10000]).