class Sys {
	
	public static function println( v : Dynamic ) {
		hl.Boot.log(v);
	}
	
	@:hlNative("std","sys_time") public static function time() : Float { return 0.; };
	@:hlNative("std","sys_exit") public static function exit( code : Int ) : Void {};
	
}