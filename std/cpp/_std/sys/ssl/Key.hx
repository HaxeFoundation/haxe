package sys.ssl;
import cpp.NativeSsl;

private typedef PKEY = Dynamic;

@:coreApi
class Key {
	
	private var __k : PKEY;

	private function new( k : PKEY ){
		__k = k;
	}
	
	public static function loadFile( file : String, ?isPublic : Bool, ?pass : String ) : Key {
		var data = sys.io.File.getBytes( file );
		var str = cpp.Lib.stringReference(data);
		if( str.indexOf("-----BEGIN ") >= 0 )
			return readPEM( str, isPublic==true, pass );
		else
			return readDER( data, isPublic==true );
	}
	
	public static function readPEM( data : String, isPublic : Bool, ?pass : String ) : Key {
		return new Key( NativeSsl.key_from_pem( data, isPublic, pass ) );
	}

	public static function readDER( data : haxe.io.Bytes, isPublic : Bool ) : Key {
		return new Key( NativeSsl.key_from_der( data.getData(), isPublic ) );
	}

	static function __init__() : Void {
		NativeSsl.init();
	}

}
