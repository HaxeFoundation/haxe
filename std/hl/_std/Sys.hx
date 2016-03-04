class Sys {
	
	public static function println( v : Dynamic ) {
		_print(@:privateAccess (Std.string(v)+"\n").bytes);
	}
	
	@:hlNative("std","sys_print") static function _print( v : hl.types.Bytes ) : Void {};
	@:hlNative("std","sys_time") public static function time() : Float { return 0.; };
	@:hlNative("std","sys_exit") public static function exit( code : Int ) : Void {};
	
}