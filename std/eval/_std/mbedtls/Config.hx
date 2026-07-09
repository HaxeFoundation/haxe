package mbedtls;

extern class Config {
	function new():Void;

	function alpn_protocols(protocols:Array<String>):Int;
	function authmode(authmode:SslAuthmode):Void;
	function ca_chain(ca_chain:X509Crt):Void;
	function defaults(endpoint:SslEndpoint, transport:SslTransport, preset:SslPreset):Int;
	function own_cert(cert:X509Crt, pk:PkContext):Int;
	function rng<T>(p_rng:T):Void;
}
