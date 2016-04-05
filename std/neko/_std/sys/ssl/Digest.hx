package sys.ssl;

@:coreApi
class Digest {
	
	public static function make( data : haxe.io.Bytes, alg : DigestAlgorithm ) : haxe.io.Bytes {
		return haxe.io.Bytes.ofData( dgst_make( data.getData(), untyped alg.__s ) );
	}
	
	public static function sign( data : haxe.io.Bytes, privKey : Key, alg : DigestAlgorithm ) : haxe.io.Bytes {
		return haxe.io.Bytes.ofData( dgst_sign( data.getData(), @:privateAccess privKey.__k, untyped alg.__s ) );
	}
	
	public static function verify( data : haxe.io.Bytes, signature : haxe.io.Bytes, pubKey : Key, alg : DigestAlgorithm ) : Bool{
		return dgst_verify( data.getData(), signature.getData(), @:privateAccess pubKey.__k, untyped alg.__s );
	}

	private static var dgst_make = neko.Lib.loadLazy("ssl","dgst_make",2);
	private static var dgst_sign = neko.Lib.loadLazy("ssl","dgst_sign",3);
	private static var dgst_verify = neko.Lib.loadLazy("ssl","dgst_verify",4);

}
