package lua;

@:native("_G.table") 
extern class TableTools {
	@:overload(function(table:Dynamic):Void{})
	public static function concat(table:Dynamic, ?sep:String) : String;

	public static function foreach(table:Dynamic, f:Dynamic->Dynamic->Void) : Void;
	public static function foreachi(table:Dynamic, f:Dynamic->Int->Void) : Void;
	public static function sort(table:Dynamic) : Void;

	@:overload(function(table:Dynamic, value:Dynamic):Void{})
	public static function insert(table:Dynamic, pos:Int, value:Dynamic) : Void;

	@:overload(function(table:Dynamic):Void{})
	public static function remove(table:Dynamic, ?pos:Int) : Void;
}
