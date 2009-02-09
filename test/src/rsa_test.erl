-module (rsa_test).

-include_lib("../include/eunit/include/eunit.hrl").

encrypt_test_() ->
	crypto:start(),
  [
    ?_assert( "blah" == rsa:encrypt("hi", "hob", "me") )
  ].
