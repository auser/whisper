-module (math).

randprime(Bits, Conf) ->
	crypto:random_uniform(Bits).