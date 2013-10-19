package rust;

@:native("sys") extern class Sys {
	/** Returns the size of a type. */
	public static function size_of<T>():Int;
	/** Initiate task failure */
	public static function begin_unwind(msg:String, file:String, line:Int):Void;
}