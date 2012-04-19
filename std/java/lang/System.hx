package java.lang;
import haxe.Int64;

/**
 * ...
 * @author waneck
 */

extern class System 
{
	static function arraycopy(src:Dynamic, srcPos:Int, dest:Dynamic, destPos:Int, length:Int):Void;
	static function currentTimeMillis():Int64;
	static function exit(status:Int):Void;
	static function getenv(name:String):String;
	static function getProperty(key:String, def:String):String;
	static function setProperty(key:String, value:String):String;
	static function load(filename:String):Void;
	static function loadLibrary(libname:String):Void;
	static function mapLibraryName(libname:String):String;
	static function gc():Void;
	static function runFinalization():Void;
	
}