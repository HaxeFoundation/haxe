package java.lang;
import haxe.Int64;

extern class System 
{
	static var err(default, null):java.io.PrintStream;
	static var out(default, null):java.io.PrintStream;
	//static var in(default, null):java.io.InputStream; //collision
	
	static function arraycopy(src:Dynamic, srcPos:Int, dest:Dynamic, destPos:Int, length:Int):Void;
	static function clearProperty(key:String):String;
	static function currentTimeMillis():Int64;
	static function exit(status:Int):Void;
	@:overload(function():java.util.Map<String,String> {})
	static function getenv(name:String):String;
	static function getProperty(key:String, ?def:String):String;
	static function identityHashCode(x:Dynamic):Int;
	static function load(filename:String):Void;
	static function loadLibrary(libname:String):Void;
	static function mapLibraryName(libname:String):String;
	static function nanoTime():Int64;
	static function gc():Void;
	static function runFinalization():Void;
	static function setErr(err:java.io.PrintStream):Void;
	static function setOut(out:java.io.PrintStream):Void;
	static function setIn(v:java.io.InputStream):Void;
	static function setProperty(key:String, value:String):String;
	
}