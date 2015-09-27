package lua;
@:native("_G.ffi")
extern class Ffi {
	public static function cdef(arg:String) : Void;
	public static function load(arg:String, global : Bool) : Dynamic;
	@:native("new")
	public static function _new(arg:String) : Dynamic;

}
