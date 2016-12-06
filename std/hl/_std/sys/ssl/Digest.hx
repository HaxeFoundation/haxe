package sys.ssl;
import sys.ssl.Lib;

@:coreApi
class Digest {
	
	public static function make( data : haxe.io.Bytes, alg : DigestAlgorithm ) : haxe.io.Bytes {
		var size = 0;
		var b = @:privateAccess dgst_make( data.b, data.length, (alg:String).toUtf8(), size );
		return @:privateAccess new haxe.io.Bytes(b,size);
	}
	
	public static function sign( data : haxe.io.Bytes, privKey : Key, alg : DigestAlgorithm ) : haxe.io.Bytes {
		var size = 0;
		var b = @:privateAccess dgst_sign( data.b, data.length, privKey.__k, (alg:String).toUtf8(), size );
		return @:privateAccess new haxe.io.Bytes(b,size);
	}
	
	public static function verify( data : haxe.io.Bytes, signature : haxe.io.Bytes, pubKey : Key, alg : DigestAlgorithm ) : Bool{
		return @:privateAccess dgst_verify( data.b, data.length, signature.b, signature.length, pubKey.__k, (alg:String).toUtf8() );
	}

	@:hlNative("ssl","dgst_make") static function dgst_make( data : hl.Bytes, len : Int, alg : hl.Bytes, size : hl.Ref<Int> ) : hl.Bytes { return null; }
	@:hlNative("ssl","dgst_sign") static function dgst_sign( data : hl.Bytes, len : Int, key : sys.ssl.Key.KeyPtr, alg : hl.Bytes, size : hl.Ref<Int> ) : hl.Bytes { return null; }
	@:hlNative("ssl","dgst_verify") static function dgst_verify( data : hl.Bytes, dlen : Int, sign : hl.Bytes, slen : Int, key : sys.ssl.Key.KeyPtr, alg : hl.Bytes ) : Bool { return false; }
	
}
