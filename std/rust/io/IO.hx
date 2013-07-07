package rust.io;

@:native("io") extern class IO {
	public static function print(s:String):Void;
	public static function println(s:String):Void;
	public static function stdin():Reader;
	public static function stdout():Writer;
}