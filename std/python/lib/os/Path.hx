
package python.lib.os;

import python.lib.Tuple;

@:pythonImport("os", "path")
extern class Path {

	public static var sep : String;
	public static function exists (path:String):Bool;

	public static function abspath (path:String):String;

	public static function basename (path:String):String;

	public static function commonprefix (paths:Array<String>):String;

	public static function lexists (path:String):Bool;

	public static function expanduser (path:String):String;

	public static function expandvars (path:String):String;

	public static function getmtime (path:String):Float;

	public static function getatime (path:String):Float;

	public static function getctime (path:String):Float;

	public static function getsize (path:String):Int;

	public static function isabs (path:String):Bool;

	public static function isfile (path:String):Bool;

	public static function isdir (path:String):Bool;

	public static function dirname (path:String):String;



	public static function islink (path:String):Bool;

	public static function ismount (path:String):Bool;

	public static function join (path:String, paths:haxe.Rest<String>):String;

	public static function normpath (path:String):String;

	public static function realpath (path:String):String;

	public static function relpath (path:String):String;

	public static function samefile (path1:String, path2:String):String;

	public static function split (path:String):Tup2<String, String>;

	public static function splitdrive (path:String):Tup2<String, String>;

	public static function splitext (path:String):Tup2<String, String>;

	public static function supports_unicode_filenames ():Bool;

}