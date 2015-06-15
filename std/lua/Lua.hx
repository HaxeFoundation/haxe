package lua;

/**
  These are all global static methods within lua
**/

@:native("_G")
extern class Lua {
	public static function unpack(x: Dynamic) : Table<Int,Dynamic>;
	public static function setmetatable(tbl: Table<Dynamic,Dynamic>, mtbl: Table<Dynamic, Dynamic>) : Void;
	public static function setfenv(i : Int, tbl: Table<Dynamic, Dynamic>) : Void;
	public static function next<T>(k:Table<Dynamic,T>, i:Null<Int>) : T;
}
