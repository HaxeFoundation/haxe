package sys.ssl;

extern class Digest {
	
	static function make( data : haxe.io.Bytes, alg : DigestAlgorithm ) : haxe.io.Bytes;
	
	static function sign( data : haxe.io.Bytes, privKey : Key, alg : DigestAlgorithm ) : haxe.io.Bytes;
	
	static function verify( data : haxe.io.Bytes, signature : haxe.io.Bytes, pubKey : Key, alg : DigestAlgorithm ) : Bool;

}
