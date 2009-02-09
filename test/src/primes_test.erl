-module (primes_test).

-include_lib("../include/eunit/include/eunit.hrl").

make_prime_test_() ->	
	crypto:start(),
  [
    ?_assert( primes:make_prime(1) =< 12 )
  ].
