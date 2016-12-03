package sys.ssl;
import sys.ssl.Lib;

@:noDoc
typedef KeyPtr = hl.Abstract<"hl_ssl_pkey">;

@:coreApi
class Key {
	
	private var __k : KeyPtr;

	private function new( k : KeyPtr ){
		__k = k;
	}
	
	public static function loadFile( file : String, ?isPublic : Bool, ?pass : String ) : Key {
		var data = sys.io.File.getBytes( file );
		var start = data.getString(0,11);
		if( start == "-----BEGIN " )
			return readPEM( data.toString(), isPublic==true, pass );
		else
			return readDER( data, isPublic==true );
	}
	
	public static function readPEM( data : String, isPublic : Bool, ?pass : String ) : Key {
		return new Key( key_from_pem( @:privateAccess data.toUtf8(), isPublic, pass == null ? null : @:privateAccess pass.toUtf8() ) );
	}

	public static function readDER( data : haxe.io.Bytes, isPublic : Bool ) : Key {
		return new Key( key_from_der( @:privateAccess data.b, @:privateAccess data.length, isPublic ) );
	}

	@:hlNative("ssl","key_from_pem") static function key_from_pem( data : hl.Bytes, pub : Bool, pass : Null<hl.Bytes> ) : KeyPtr { return null; }
	@:hlNative("ssl","key_from_der") static function key_from_der( data : hl.Bytes, len : Int, pub : Bool ) : KeyPtr { return null; }

}
