package cs.system;

/** Specifies options for a . */
@:native("System.GenericUriParserOptions")
extern enum abstract GenericUriParserOptions(Int) {
	var AllowEmptyAuthority = 2;
	var Default = 0;
	var DontCompressPath = 128;
	var DontConvertPathBackslashes = 64;
	var DontUnescapePathDotsAndSlashes = 256;
	var GenericAuthority = 1;
	var Idn = 512;
	var IriParsing = 1024;
	var NoFragment = 32;
	var NoPort = 8;
	var NoQuery = 16;
	var NoUserInfo = 4;
	@:op(A | B) static function or(lhs:GenericUriParserOptions, rhs:GenericUriParserOptions):GenericUriParserOptions;
	@:op(A & B) static function and(lhs:GenericUriParserOptions, rhs:GenericUriParserOptions):GenericUriParserOptions;
	@:op(A ^ B) static function xor(lhs:GenericUriParserOptions, rhs:GenericUriParserOptions):GenericUriParserOptions;
	@:op(~A) static function complement(value:GenericUriParserOptions):GenericUriParserOptions;
}
