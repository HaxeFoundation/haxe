import rust.BaseIter;
@:native("vec") @:nativeGen extern class Array<T> implements ArrayAccess<T> implements BaseIter<T> {
	public var length(get, null):Int;

	public static function capacity<T>(n:Array<T>):Int;
	public static function len<T>(n:Array<T>):Int;
	public static function filter<T>(n:Array<T>, f:T -> Bool):Array<T>;
	public static function from_elem<T>(len:Int, d:T):Array<T>;
	public static function grow<T>(v:Array<T>, n:Int, initv:T):Array<T>;
	public static function head<T>(v:Array<T>):T;
	public static function is_empty<T>(v:Array<T>):Bool;
	public function each(f:T -> Bool):Void;
	public function size_hint():Null<Int>;
}