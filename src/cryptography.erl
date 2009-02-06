-module (cryptography).

-export ([encrypt/3, decrypt/3]).

encrypt(Key, IV, Data) ->
	crypto:des_cbc_encrypt(Key, IV, Data).

decrypt(Key, IV, Data) ->
	crypto:des_cbc_decrypt(Key, IV, Data).