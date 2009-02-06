-module (cryptography_test).

-include_lib("../include/eunit/include/eunit.hrl").

encrypt_test_() ->
	crypto:start(),
  [
    ?_assert( "blah" == cryptography:encrypt("hi", "hob", "me") )
  ].
