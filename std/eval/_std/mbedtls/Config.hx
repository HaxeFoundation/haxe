package mbedtls;

extern class Config {
	function new():Void;

	function authmode(authmode:SslAuthmode):Void;
	function ca_chain(ca_chain:Certificate):Void;
	function defaults(endpoint:SslEndpoint, transport:SslTransport, preset:SslPreset):Int;
	function rng<T>(p_rng:T):Void;
}
