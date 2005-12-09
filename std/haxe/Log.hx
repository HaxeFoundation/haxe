class Log {

	public static function trace( v : Dynamic ) : Void {
		#flash
		untyped Boot.__trace(v);
		#else neko
		untyped __dollar__print(v,"\n");
		#else error
		#end
	}

	public static function clear() : Void {
		#flash
		untyped Boot.__clear_trace();
		#else neko
		// nothing
		#else error
		#end
	}

}