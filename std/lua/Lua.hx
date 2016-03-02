package lua;
import haxe.extern.Rest;

/**
  These are all global static methods within lua
 **/

@:native("_G")
extern class Lua {
	public static var _VERSION : String;
	public static function getmetatable(tbl: Table<Dynamic,Dynamic>): Table<Dynamic,Dynamic>;
	public static function setmetatable(tbl: Table<Dynamic,Dynamic>, mtbl: Table<Dynamic, Dynamic>): Void;
	public static function setfenv(i: Int , tbl: Table<Dynamic, Dynamic>): Void;
	public static function next<T>(k : Table<Dynamic, T>, ?i : Null<Int>): T;
	public static function tostring(v: Dynamic): String;
	public static function ipairs<T>(t: Table<Int,T>): Void->T;
	public static function pairs<A,B>(t: Table<A,B>): Void->A;
	public static function tonumber(str:String, ?base:Int): Int;
	public static function type(v:Dynamic): String;
	public static function print(v:Dynamic) : Void;
	public static function select(n:Dynamic, rest:Rest<Dynamic>) : Dynamic;
	public static function rawget<K,V>(t:Table<K,V>, k:K) : V;
	public static function rawset<K,V>(t:Table<K,V>, k:K, v:V) : Void;
	private static function __init__() : Void {
		// print polyfill
		haxe.macro.Compiler.includeFile("lua/_lua/_hx_print.lua");
	}
}
