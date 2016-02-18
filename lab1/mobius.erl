-module(mobius).
-export([is_prime/1]).

is_prime(N) ->
is_prime_sub_func(N, trunc(math:sqrt(N))). %% trunc - усечение float

is_prime_sub_func(1, _) -> true; %% заглушка для 1
is_prime_sub_func(2, _) -> true; %% 2 тоже простое число
is_prime_sub_func(_, 1) -> true; %% делится только на 1 -> простое число
is_prime_sub_func(T, N) when T rem N =:= 0 -> false;
is_prime_sub_func(T, N) when T rem N =/= 0 -> is_prime_sub_func(T, N-1).