package lua;
import haxe.extern.Rest;
import haxe.Constraints.Function;
import haxe.extern.Rest;

/**
  These are all global static methods within lua
 **/

@:native("_G")
extern class Lua {
	public static var _VERSION : String;
	public static var arg : Table<Int, String>;
	public static function getmetatable(tbl:Table<Dynamic,Dynamic>): Table<Dynamic,Dynamic>;
	public static function setmetatable(tbl:Table<Dynamic,Dynamic>, mtbl: Table<Dynamic, Dynamic>): Void;
	public static function setfenv(i:Int , tbl:Table<Dynamic, Dynamic>): Void;
	public static function next<T>(k:Table<Dynamic, T>, ?i : Null<Int>): T;
	public static function tostring(v:Dynamic): String;
	public static function ipairs<T>(t:Table<Int,T>): Void->T;
	public static function pairs<A,B>(t:Table<A,B>): Void->A;
	public static function tonumber(str:String, ?base:Int): Int;
	public static function type(v:Dynamic) : String;
	public static function print(v:Dynamic) : Void;
	public static function select(n:Dynamic, rest:Rest<Dynamic>) : Dynamic;
	public static function rawget<K,V>(t:Table<K,V>, k:K) : V;
	public static function rawset<K,V>(t:Table<K,V>, k:K, v:V) : Void;
	public static function collectgarbage(opt:CollectGarbageOption, ?arg:Int) : Int;
	public static function assert(v:Bool, message:String) : Bool;
	public static function dofile(filename:String) : Void;
	public static function error(message:String, ?level:Int) : Void;
	public static function pcall(f:Function, rest:Rest<Dynamic>) : Bool;
	public static function rawequal(v1:Dynamic, v2:Dynamic) : Bool;
	public static function xpcall(f:Function, msgh:Function, rest:Rest<Dynamic> ) : Bool;
	public static function loadfile(filename:String) : Void;
	public static function loadstring(code:String) : Void;

	private static function __init__() : Void {
		// print polyfill
		haxe.macro.Compiler.includeFile("lua/_lua/_hx_print.lua");
	}
}

/**
  Enum for describing garbage collection options
  */
@:enum
abstract CollectGarbageOption(String) {
	var Stop = "stop";
	var Restart = "restart";
	var Collect = "collect";
	var Count = "count";
	var Step = "step";
	var SetPause = "setpause";
	var SetStepMul = "setstepmul";
}
