import rust.os.*;
import rust.io.*;
import rust.Lib;
@:coreApi class Sys {
	public static function args():Array<String> {
		return OS.args();
	}
	@:functionCode('io::print(s)')
	public static inline function print(s:String):Void {}
	@:functionCode('io::println(s)')
	public static inline function println(s:String):Void {}
	public static function getEnv(s:String):String {
		return OS.getenv(s);
	}
	public static function putEnv(s:String, v:String):Void {
		OS.setenv(s, v);
	}
	public static function environment():haxe.ds.StringMap<String> {
		return null;
	}
	public static function sleep(s:Float):Void {

	}
	public static function command(cmd:String, ?args:Array<String>):Int {
		return 0;
	}
	public static function getCwd():String {
		return "";
	}
	public static function cpuTime():Float {
		return 0;
	}
	public static function exit(c:Int):Void {

	}
}