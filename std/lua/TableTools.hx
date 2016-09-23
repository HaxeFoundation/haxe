
package lua;

/**
	This library provides generic functions for table manipulation.
**/
@:native("_hx_table")
extern class TableTools {
	public static function pack<T>(args:haxe.extern.Rest<T>) : Table<Int,T>;
	public static function unpack<Int,V>(args:lua.Table<Int,V>, ?min : Int, ?max : Int) : Dynamic;
	public static function maxn(t:Table.AnyTable) : Int;
	public static function __init__() : Void {
	  untyped _hx_table = __define_feature__("use._hx_table", _hx_table);
	}
}
