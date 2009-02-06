-module (math).

make_prime(K) when K > 0 ->
    new_seed(),
    N = make(K),
    if N > 3 ->
            io:format("Generating a ~w digit prime ",[K]),
            MaxTries = N - 3,
            P1 = make_prime(MaxTries, N+1),
            io:format("~n",[]),
            P1;
        true ->
            make_prime(K)
    end.

make_prime(0, _) ->
    exit(impossible);
make_prime(K, P) ->
    io:format(".",[]),
    case is_prime(P) of
        true  -> P;
        false -> make_prime(K-1, P+1)
    end.

new_seed() ->
	{_,_,X} = erlang:now(),
	{H,M,S} = time(),
	H1 = H * X rem 32767,
	M1 = M * X rem 32767,
	S1 = S * X rem 32767,
	{random_seed, {H1,M1,S1}}.

