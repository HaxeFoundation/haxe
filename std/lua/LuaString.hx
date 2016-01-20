package lua;
/**
  These are all externs for the base Lua "string" class.  Note that all
  relevant indexes are "1" based.
 **/
@:native("_G.string")
extern class LuaString {
	public static function len(str : String): Int;
	public static function char(i: Int): String;
	public static function sub(str : String, start : Int, ?end : Int): String;
	public static function charCodeAt(str : String, index : Int): Int;
	public static function find(str : String, target : String, ?start : Int, ?plain : Bool): Int;
	public static function byte(str : String, ?index : Int) : Int;
	public static function format(str : String, ?e1 : Dynamic, ?e2 : Dynamic, ?e3 : Dynamic, ?e4 : Dynamic): String;

	@:overload(   function     (str : String, pattern : String, replace : String->Void,   ?n : Int): String {})
	@:overload(   function     (str : String, pattern : String, replace : String->String, ?n : Int): String {})
	public static function gsub(str : String, pattern : String, replace : String,		  ?n : Int): String;

	@:overload(   function     (str : String, pattern : String, match : Void->String,   ?n : Int): String->Void {})
	public static function gmatch(str : String, pattern : String, ?n : Int): Void->String;

	public static function upper(str:String) : String;
	public static function lower(str:String) : String;
}

