package lua;
/**
  These are all externs for the base Lua "string" class.  Note that all
  relevant indexes are "1" based.
 **/
@:native("_G.string")
extern class StringTools {
	public static function len(str : String): Int;
	public static function sub(str : String, start : Int, end : Int): String;
	public static function charCodeAt(str : String, index : Int): Int;
	public static function find(str : String, target : String, start : Int, end : Int, plain : Bool): Int;
	public static function byte(str : String, ?index : Int) : Int;
	public static function format(str : String, ?e1 : String, ?e2 : String, ?e3 : String, ?e4 : String): String;

	@:overload(   function     (str : String, pattern : String, replace : String->Void,   ?n : Int): String {})
	@:overload(   function     (str : String, pattern : String, replace : String->String, ?n : Int): String {})
	public static function gsub(str : String, pattern : String, replace : String,		  ?n : Int): String;
}

