package sys.ssl;

@:enum
abstract DigestAlgorithm(String) {
	var MD5 = "MD5";
	var SHA1 = "SHA1";
	var SHA224 = "SHA224";
	var SHA256 = "SHA256";
	var SHA384 = "SHA384";
	var SHA512 = "SHA512";
	var RIPEMD160 = "RIPEMD160";
}

extern class Digest {
	
	static function make( data : haxe.io.Bytes, alg : DigestAlgorithm ) : haxe.io.Bytes;
	
	static function sign( data : haxe.io.Bytes, privKey : Key, alg : DigestAlgorithm ) : haxe.io.Bytes;
	
	static function verify( data : haxe.io.Bytes, signature : haxe.io.Bytes, pubKey : Key, alg : DigestAlgorithm ) : Bool;

}
