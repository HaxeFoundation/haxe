package lua;

/**
	This library provides generic functions for table manipulation.
**/
@:native("_G.table")
extern class Table<A,B> implements ArrayAccess<B> implements Dynamic<B> {
	public inline static function create<A,B>(?arr:Array<B>, ?hsh:Dynamic<B>) : Table<A,B> {
		return untyped __lua_table__(arr,hsh);
	}

	@:overload(function<A,B>(table:Table<A,B>):Void{})
	public static function concat<A,B>(table:Table<A,B>, ?sep:String) : String;

	public static function foreach<A,B>(table:Table<A,B>, f:A->B->Void) : Void;
	public static function foreachi<A,B>(table:Table<A,B>, f:A->B->Int->Void) : Void;

	public static function sort<A,B>(table:Table<A,B>, ?order : A->A->Bool) : Void;

	@:overload(function<B>(table:Table<Int,B>, value:B):Void{})
	public static function insert<B>(table:Table<Int,B>, pos:Int, value:B) : Void;

	@:overload(function<B>(table:Table<Int,B>):Void{})
	public static function remove<B>(table:Table<Int,B>, ?pos:Int) : Void;

	public static function maxn<B>(table: Table<Int,B>) : Int;
	public static function pack<T>(args:T) : Table<Int,T>;
	public static function unpack(arg:Table<Dynamic,Dynamic>, ?min:Int, ?max:Int) : Dynamic;
	private static function __init__() : Void {
		// lua table polyfills
		haxe.macro.Compiler.includeFile("lua/_lua/_hx_table_polyfill.lua");
	}
}

typedef AnyTable = Table<Dynamic, Dynamic>;
