package cs.system.net.security;

/** Enumerates Secure Socket Layer (SSL) policy errors. */
@:native("System.Net.Security.SslPolicyErrors")
extern enum abstract SslPolicyErrors(Int) {
	var None = 0;
	var RemoteCertificateChainErrors = 4;
	var RemoteCertificateNameMismatch = 2;
	var RemoteCertificateNotAvailable = 1;
	@:op(A | B) static function or(lhs:SslPolicyErrors, rhs:SslPolicyErrors):SslPolicyErrors;
	@:op(A & B) static function and(lhs:SslPolicyErrors, rhs:SslPolicyErrors):SslPolicyErrors;
	@:op(A ^ B) static function xor(lhs:SslPolicyErrors, rhs:SslPolicyErrors):SslPolicyErrors;
	@:op(~A) static function complement(value:SslPolicyErrors):SslPolicyErrors;
}
