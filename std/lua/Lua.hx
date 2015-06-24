package lua;

/**
  These are all global static methods within lua
 **/

@:native("_G")
extern class Lua {
	public static function setmetatable(tbl: Table<Dynamic,Dynamic>, mtbl: Table<Dynamic, Dynamic>): Void;
	public static function setfenv(i: Int , tbl: Table<Dynamic, Dynamic>): Void;
	public static function next<T>(k : Table<Dynamic, T>, ?i : Null<Int>): T;
	public static function tostring(v: Dynamic): String;
	public static function ipairs<T>(t: Table<Int,T>): Void->T;
	public static function pairs<A,B>(t: Table<A,B>): Void->A;
}
