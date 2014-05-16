package python.lib;

import python.lib.Exceptions.BaseException;
import python.lib.io.FileIO;
import python.lib.io.RawIOBase;
import python.lib.io.TextIOBase;
import python.lib.Tuple.Tup3;


extern class TB {}
extern class Frame {}

@:pythonImport("sys")
extern class Sys {

	public static var argv(default, never):Array<String>;

	public static function exit (x:Int):Void;

	public static function getfilesystemencoding():String;

	public static var version:String;
	public static var platform:String;

	public static var stdout(default, never):TextIOBase;
	public static var stdin(default, never):TextIOBase;
	public static var stderr(default, never):TextIOBase;

	public static function getsizeof (t:Dynamic):Int;

	public static var maxsize:Int;

	public static function exc_info<T:BaseException>():Tup3<Class<T>, T, TB>;

}
