package lua;

@:native("_G.table") 
extern class Table<A,B> implements ArrayAccess<B> {
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
}
