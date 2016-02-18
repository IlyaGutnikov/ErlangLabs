-module(mobius).
-export([is_prime/1, prime_factors/1, is_square_multiple/1, find_duplication/2]).

is_prime(N) ->
is_prime_sub_func(N, trunc(math:sqrt(N))). %% trunc - усечение float

is_prime_sub_func(1, _) -> true; %% заглушка для 1
is_prime_sub_func(2, _) -> true; %% 2 тоже простое число
is_prime_sub_func(_, 1) -> true; %% делится только на 1 -> простое число
is_prime_sub_func(T, N) when T rem N =:= 0 -> false;
is_prime_sub_func(T, N) when T rem N =/= 0 -> is_prime_sub_func(T, N-1).

prime_factors(N) -> prime_factors_sub_func(N, [], 2).

prime_factors_sub_func(1, [], _) -> [1];
prime_factors_sub_func(N, Result, N) -> Result ++ [N];
prime_factors_sub_func(N, Result, M) when N rem M =/= 0 -> prime_factors_sub_func(N, Result, M+1);
prime_factors_sub_func(N, Result, M) when N rem M =:= 0 -> prime_factors_sub_func(N div M, Result ++ [M] , 2).

is_square_multiple(N) -> find_duplication(prime_factors(N), lists:usort(prime_factors(N))).

find_duplication(X, Y) ->
    if
        X =/= Y -> true;
        true -> false
    end.