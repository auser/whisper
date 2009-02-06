-module(rsa_key).

-export([make_sig/1, make_sig/2]).

make_sig(Who, Len) when Len > 79 ->
    {{Pub,N},{Priv,N}}={Public, Private} = make_sig(Len),
    file:write_file(Who ++ ".pub", term_to_binary(Public)),
    file:write_file(Who ++ ".pri", term_to_binary(Private)),
    D=str2int("asdfasdfasdfad"),
    E=crypto:mod_exp(D,Priv,N),
    F=crypto:mod_exp(E,Pub,N),
    io:format("String ~p,~p~n",[int2str(F),F]),
    {term_to_binary(Public),term_to_binary(Private)}.

%S tag1
make_sig(Len) ->
    %% generate two <Len> digit prime numbers
    P = primes:make_prime(Len),
    io:format("P = ~p~n", [P]),
    Q = primes:make_prime(Len),
    io:format("Q = ~p~n", [Q]),
    N = P*Q,
    io:format("N = ~p~n", [N]),
    Phi = (P-1)*(Q-1),
    %% now make B such that B < Phi and gcd(B, Phi) = 1
    B = b(Phi),
    io:format("Public key (B) = ~p~n", [B]),
    A = inv(B, Phi),
    io:format("Private key (A) = ~p~n", [A]),
    {{B,N},{A,N}}.

b(Phi) ->
    io:format("Generating a public key B "),
    K = length(integer_to_list(Phi)) - 1,
    B = b(1, K, Phi),
    io:format("~n", []),
    B.

b(Try, K, Phi) ->
    io:format("."),
    B = primes:make(K),
    if 
	B < Phi ->
	    case gcd(B, Phi) of
		1 -> B;
		_ -> b(Try+1, K, Phi)
	    end;
	true ->
	    b(Try, K, Phi)
    end.
%E tag1

inv(A, B) ->
    case solve(A, B) of
	{X, Y} ->
	    if X < 0 -> X + B;
	       true  -> X
	    end;
	_ ->
	    no_inverse
    end.

%% greatest common denominator
gcd(A, B) when A < B -> gcd(B, A);
gcd(A, 0) -> A;
gcd(A, B) ->
    gcd(B, A rem B).



solve(A, B) ->
    case catch s(A,B) of
	insoluble -> insoluble;
	{X, Y} ->
	    case A * X - B * Y of
		1     -> {X, Y};
		Other -> error
	    end
    end.

s(A, 0)  -> throw(insoluble);
s(A, 1)  -> {0, -1};
s(A, -1) -> {0, 1};
s(A, B)  ->
    K1 = A div B,
    K2 = A - K1*B,
    {Tmp, X} = s(B, -K2),
    {X, K1 * X - Tmp}.


%% converts a string to a base 256 integer
%% converts a base 256 integer to a string

%S tag2    
str2int(Str) -> str2int(Str, 0).

str2int([H|T], N) -> str2int(T, N*256+H);
str2int([], N) -> N.

int2str(N) -> int2str(N, []).

int2str(N, L) when N =< 0 -> L;
int2str(N, L) ->
    N1 = N div 256,
    H = N - N1 * 256,
    int2str(N1, [H|L]).
%E tag2