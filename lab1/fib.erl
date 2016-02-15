-module(fib).
-export([fib_p/1, fib_g/1]).

fib_p(0) -> 0;
fib_p(1) -> 1;
fib_p(N) -> fib_p(N-1) + fib_p(N-2).

fib_g(N) when N == 0 -> 0 ;
fib_g(N) when N == 1 -> 1;
fib_g(N) -> fib_g(N-1) + fib_g(N-2).