package cs.system.net;

/** Specifies protocols for authentication. */
@:native("System.Net.AuthenticationSchemes")
extern enum abstract AuthenticationSchemes(Int) {
	var Anonymous = 32768;
	var Basic = 8;
	var Digest = 1;
	var IntegratedWindowsAuthentication = 6;
	var Negotiate = 2;
	var None = 0;
	var Ntlm = 4;
	@:op(A | B) static function or(lhs:AuthenticationSchemes, rhs:AuthenticationSchemes):AuthenticationSchemes;
	@:op(A & B) static function and(lhs:AuthenticationSchemes, rhs:AuthenticationSchemes):AuthenticationSchemes;
	@:op(A ^ B) static function xor(lhs:AuthenticationSchemes, rhs:AuthenticationSchemes):AuthenticationSchemes;
	@:op(~A) static function complement(value:AuthenticationSchemes):AuthenticationSchemes;
}
