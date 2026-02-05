package cs.system;

/** Specifies the parts of a . */
@:native("System.UriComponents")
extern enum abstract UriComponents(Int) {
	var AbsoluteUri = 127;
	var Fragment = 64;
	var Host = 4;
	var HostAndPort = 132;
	var HttpRequestUrl = 61;
	var KeepDelimiter = 1073741824;
	var NormalizedHost = 256;
	var Path = 16;
	var PathAndQuery = 48;
	var Port = 8;
	var Query = 32;
	var Scheme = 1;
	var SchemeAndServer = 13;
	var SerializationInfoString = -2147483648;
	var StrongAuthority = 134;
	var StrongPort = 128;
	var UserInfo = 2;
	@:op(A | B) static function or(lhs:UriComponents, rhs:UriComponents):UriComponents;
	@:op(A & B) static function and(lhs:UriComponents, rhs:UriComponents):UriComponents;
	@:op(A ^ B) static function xor(lhs:UriComponents, rhs:UriComponents):UriComponents;
	@:op(~A) static function complement(value:UriComponents):UriComponents;
}
