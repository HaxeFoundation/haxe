#flash
import Boot;
#end

class Std {

	public static function instanceof( obj : Dynamic, vclass : Dynamic ) : Bool {
		return untyped
		#flash
		Boot.__instanceof(obj,vclass);
		#else error
		#end
	}

	public static function string( s : Dynamic ) : String {
		return untyped
		#flash
		String(s);
		#else error
		#end
	}

}
