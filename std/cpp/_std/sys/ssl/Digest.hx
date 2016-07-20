package sys.ssl;
import cpp.NativeSsl;

@:coreApi
class Digest {

	public static function make( data : haxe.io.Bytes, alg : DigestAlgorithm ) : haxe.io.Bytes {
		return haxe.io.Bytes.ofData( NativeSsl.dgst_make( data.getData(), alg ) );
	}
	
	public static function sign( data : haxe.io.Bytes, privKey : Key, alg : DigestAlgorithm ) : haxe.io.Bytes {
		return haxe.io.Bytes.ofData( NativeSsl.dgst_sign( data.getData(), @:privateAccess privKey.__k, alg ) );
	}
	
	public static function verify( data : haxe.io.Bytes, signature : haxe.io.Bytes, pubKey : Key, alg : DigestAlgorithm ) : Bool{
		return NativeSsl.dgst_verify( data.getData(), signature.getData(), @:privateAccess pubKey.__k, alg );
	}

}
