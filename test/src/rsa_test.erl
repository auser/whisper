-module (rsa_test).

-include_lib("../include/eunit/include/eunit.hrl").

encrypt_test_() ->
	application:start(crypto),
	A = 3767742983994034865,B = 7634266898377977425, N = 71252294487091383773,
  [
    ?_assert( 988485182823521954 == rsa:encrypt({A,N},"hi") )
  ].
