package mbedtls;

extern class Config {
	function new():Void;

	function authmode(authmode:SslAuthmode):Void;
	function defaults(endpoint:SslEndpoint, transport:SslTransport, preset:SslPreset):Int;
	function free():Void;
	function rng<T>(p_rng:T):Void;
}
