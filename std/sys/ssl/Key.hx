package sys.ssl;

extern class Key {
	
	static function loadFile( file : String, ?isPublic : Bool, ?pass : String ) : Key;
	
	static function readPEM( data : String, isPublic : Bool, ?pass : String ) : Key;

	static function readDER( data : haxe.io.Bytes, isPublic : Bool ) : Key;

}
