-module (rsa).

-export([encrypt/1, decrypt/1, 
	decrypt_from_file/1, encrypt_to_file/2, save_key/3, load_key/2]).

-export ([init/0]).
% Returns a public and private key
init() ->
	application:start(crypto).
	
secret_key() ->
  "averylongstringthat1st0ta11ys3cr3tt0th33nt1r3w0rldsodontshareitwithanyone".
 
encrypt(Opaque)->
	BinOpaque = term_to_binary(Opaque),
	ListOpaque = binary_to_list(BinOpaque),
	MAC = crypto:sha_mac(secret_key(),BinOpaque),
	Final=encode_base64(ListOpaque)++"--"++encode_base64(binary_to_list(MAC)),
	if length(Final) > 4096 ->
		{error, toobig};
	 true -> Final
	end.

decrypt(String) ->
	[BinOpaque64, MACL64] = string:tokens(String, "--"),
	BinOpaque = decode_base64(BinOpaque64),
	MAC = list_to_binary(decode_base64(MACL64)),
	NewMAC = crypto:sha_mac(secret_key(),BinOpaque),
	if  NewMAC == MAC -> binary_to_term(list_to_binary(BinOpaque));
	   true -> tampered
	end.

	  
decrypt_from_file(Filename) ->
    {ok, [{ciphertext, Data}]} = file:consult(Filename),
    decrypt(Data).

encrypt_to_file(Filename, Msg) ->
    {ok, FP} = file:open(Filename, [write]),
    io:format(FP, "{ciphertext, ~p}.~n", [encrypt(Msg)]),
    file:close(FP).

save_key(Filename, Type, Key) ->
    {ok, FP} = file:open(Filename, [write]),
    io:format(FP, "{~p, ~p}.~n", [Type, Key]),
    file:close(FP).

load_key(Filename, Type) ->
    {ok, Terms} = file:consult(Filename),
    {value, {Type, Ret}} = lists:keysearch(Type, 1, Terms),
    Ret.

decode_base64(S) ->
    decode1_base64([C || C <- S,
			 C /= $ ,
			 C /= $\t,
			 C /= $\n,
			 C /= $\r]).

decode1_base64([]) ->
    [];
decode1_base64([Sextet1,Sextet2,$=,$=|Rest]) ->
    Bits2x6=
	(d(Sextet1) bsl 18) bor
	(d(Sextet2) bsl 12),
    Octet1=Bits2x6 bsr 16,
    [Octet1|decode_base64(Rest)];
decode1_base64([Sextet1,Sextet2,Sextet3,$=|Rest]) ->
    Bits3x6=
	(d(Sextet1) bsl 18) bor
	(d(Sextet2) bsl 12) bor
	(d(Sextet3) bsl 6),
    Octet1=Bits3x6 bsr 16,
    Octet2=(Bits3x6 bsr 8) band 16#ff,
    [Octet1,Octet2|decode_base64(Rest)];
decode1_base64([Sextet1,Sextet2,Sextet3,Sextet4|Rest]) ->
    Bits4x6=
	(d(Sextet1) bsl 18) bor
	(d(Sextet2) bsl 12) bor
	(d(Sextet3) bsl 6) bor
	d(Sextet4),
    Octet1=Bits4x6 bsr 16,
    Octet2=(Bits4x6 bsr 8) band 16#ff,
    Octet3=Bits4x6 band 16#ff,
    [Octet1,Octet2,Octet3|decode_base64(Rest)];
decode1_base64(_CatchAll) ->
    "".

d(X) when X >= $A, X =<$Z ->
    X-65;
d(X) when X >= $a, X =<$z ->
    X-71;
d(X) when X >= $0, X =<$9 ->
    X+4;
d($+) -> 62;
d($/) -> 63;
d(_) -> 63.


encode_base64([]) ->
    [];
encode_base64([A]) ->
    [e(A bsr 2), e((A band 3) bsl 4), $=, $=];
encode_base64([A,B]) ->
    [e(A bsr 2), e(((A band 3) bsl 4) bor (B bsr 4)), e((B band 15) bsl 2), $=];
encode_base64([A,B,C|Ls]) ->
    encode_base64_do(A,B,C, Ls).
encode_base64_do(A,B,C, Rest) ->
    BB = (A bsl 16) bor (B bsl 8) bor C,
    [e(BB bsr 18), e((BB bsr 12) band 63), 
     e((BB bsr 6) band 63), e(BB band 63)|encode_base64(Rest)].

e(X) when X >= 0, X < 26 -> X+65;
e(X) when X>25, X<52 ->     X+71;
e(X) when X>51, X<62 ->     X-4;
e(62) ->                    $+;
e(63) ->                    $/;
e(X) ->                     exit({bad_encode_base64_token, X}).