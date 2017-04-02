package lua;

/**
	This library provides generic functions for table manipulation.
**/
// TODO: use an abstract here?
@:native("_G.table")
extern class Table<A,B> implements ArrayAccess<B> implements Dynamic<B> {

	@:analyzer(no_fusion)
	public inline static function create<A,B>(?arr:Array<B>, ?hsh:Dynamic) : Table<A,B> {
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

#if (lua_ver >= 5.2)
	public static function pack<T>(args:haxe.extern.Rest<T>) : Table<Int,T>;
	public static function unpack<Int,V>(args:lua.Table<Int,V>, ?min : Int, ?max : Int) : Dynamic;
#end

}

typedef AnyTable = Table<Dynamic, Dynamic>;
