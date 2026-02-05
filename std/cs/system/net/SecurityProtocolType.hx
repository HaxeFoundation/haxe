package cs.system.net;

/** Specifies the security protocols that are supported by the Schannel security package. */
@:native("System.Net.SecurityProtocolType")
extern enum abstract SecurityProtocolType(Int) {
	var Ssl3 = 48;
	var SystemDefault = 0;
	var Tls = 192;
	var Tls11 = 768;
	var Tls12 = 3072;
	@:op(A | B) static function or(lhs:SecurityProtocolType, rhs:SecurityProtocolType):SecurityProtocolType;
	@:op(A & B) static function and(lhs:SecurityProtocolType, rhs:SecurityProtocolType):SecurityProtocolType;
	@:op(A ^ B) static function xor(lhs:SecurityProtocolType, rhs:SecurityProtocolType):SecurityProtocolType;
	@:op(~A) static function complement(value:SecurityProtocolType):SecurityProtocolType;
}
