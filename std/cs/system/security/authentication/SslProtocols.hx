package cs.system.security.authentication;

/** Defines the possible versions of . */
@:native("System.Security.Authentication.SslProtocols")
extern enum abstract SslProtocols(Int) {
	var Default = 240;
	var None = 0;
	var Ssl2 = 12;
	var Ssl3 = 48;
	var Tls = 192;
	var Tls11 = 768;
	var Tls12 = 3072;
	@:op(A | B) static function or(lhs:SslProtocols, rhs:SslProtocols):SslProtocols;
	@:op(A & B) static function and(lhs:SslProtocols, rhs:SslProtocols):SslProtocols;
	@:op(A ^ B) static function xor(lhs:SslProtocols, rhs:SslProtocols):SslProtocols;
	@:op(~A) static function complement(value:SslProtocols):SslProtocols;
}
