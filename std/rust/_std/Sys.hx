import rust.OS;
import rust.io.*;
import rust.Lib;
import haxe.ds.StringMap;
import haxe.io.*;
import rust.io.*;
class Sys {
	public static inline function stdin():Input {
		return new NativeInput(IO.stdin());
	}
	public static inline function stdout():Output {
		return new NativeOutput(IO.stdout());
	}
	public static function args():Array<String> {
		return OS.args();
	}
	public static inline function print(s:String):Void {
		IO.print(s);
	}
	public static inline function println(s:String):Void {
		IO.println(s);
	}
	public static function getEnv(s:String):String {
		return OS.getenv(s);
	}
	public static function putEnv(s:String, v:String):Void {
		OS.setenv(s, v);
	}
	public static function environment():haxe.ds.StringMap<String> {
		return new StringMap();
	}
	public static function sleep(s:Float):Void {

	}
	public static function command(cmd:String, ?args:Array<String>):Int {
		return 0;
	}
	public static function getCwd():String {
		return "";
	}
	@:functionCode('return std::time::precise_time_s()')
	public static function cpuTime():Float {
		return 0;
	}
	public static function exit(c:Int):Void {

	}
	@:functionCode('return std::time::precise_time_s()')
	public static function time():Float {
		return 0;
	}
}