package sys.ssl;

private typedef PKEY = Dynamic;

@:coreApi
class Key {
	
	private var __k : PKEY;

	private function new( k : PKEY ){
		__k = k;
	}
	
	public static function loadFile( file : String, ?isPublic : Bool, ?pass : String ) : Key {
		var data = sys.io.File.getBytes( file );
		var str = neko.Lib.stringReference(data);
		if( str.indexOf("-----BEGIN ") >= 0 )
			return readPEM( str, isPublic==true, pass );
		else
			return readDER( data, isPublic==true );
	}
	
	public static function readPEM( data : String, isPublic : Bool, ?pass : String ) : Key {
		return new Key( key_from_pem( untyped data.__s, isPublic, pass == null ? null : untyped pass.__s ) );
	}

	public static function readDER( data : haxe.io.Bytes, isPublic : Bool ) : Key {
		return new Key( key_from_der( data.getData(), isPublic ) );
	}

	private static var key_from_pem = neko.Lib.loadLazy("ssl","key_from_pem",3);
	private static var key_from_der = neko.Lib.loadLazy("ssl","key_from_der",2);

}
