import Boot;

class Std {

	public static function instanceof( obj : Dynamic, vclass : Dynamic ) : Bool {
		return untyped
		#flash
		Boot.__instanceof(obj,vclass);
		#else neko
		Boot.__instanceof(obj,vclass);
		#else error
		#end
	}

	public static function string( s : Dynamic ) : String {
		return untyped
		#flash
		Boot.__string_rec(s,"");
		#else neko
		__dollar__string(s);
		#else error
		#end
	}

}
