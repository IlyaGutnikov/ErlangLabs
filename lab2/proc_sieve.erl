-module(proc_sieve).
-export([generate/1]).
-export([sieve_sub_func/2]).
-export([gen_print/1]).

generate(MaxN) ->
        Pid = sieve(),
        generate_sub_func(Pid, 2, MaxN).

generate_sub_func(Pid, End, End) ->
        Pid ! {done, self()},
        receive
                Res -> Res
        end;

generate_sub_func(Pid, N, End) ->
        Pid ! N,
        generate_sub_func(Pid, N + 1, End).

sieve() ->
	%%Модуль, функция, аргументы
    spawn(proc_sieve, sieve_sub_func, [0, void]).

sieve_sub_func(0, InvalidPid) ->
    receive 
        P -> sieve_sub_func(P, InvalidPid)
    end; 

sieve_sub_func(P, NextPid) when is_pid(NextPid) ->
    receive 
        {done, From} ->
            NextPid ! {done, self()},
            receive 
                LstOfRes -> 
                    From ! [P] ++ LstOfRes 
            end;
        N when N rem P == 0 -> 
            sieve_sub_func(P, NextPid);
        N when N rem P /= 0 -> 
            NextPid ! N,
            sieve_sub_func(P, NextPid)
    end;

sieve_sub_func(P, Invalid) ->
    receive 
        {done, From} ->
            From ! [P];
        N when N rem P == 0 -> 
            sieve_sub_func(P, Invalid);
        N when N rem P /= 0 -> 
            Pid = spawn(proc_sieve, sieve_sub_func, [0, void]),
            Pid ! N,
            sieve_sub_func(P, Pid)
    end.

gen_print(MaxN) -> lists:foreach(
	fun(X)->io:format("~w ",[X]) end,
	generate(MaxN)).