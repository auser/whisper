% send
whisper:start(normal, []).
String = "Welcome back Kotter".
EncryptedMessage = whisper:encrypt(String).
Term = {data, EncryptedMessage}.
Packet = term_to_binary(Term). % converse
% received
{data, Bin} = binary_to_term(Packet). % converse
DecryptedMessage = whisper:decrypt(Bin).