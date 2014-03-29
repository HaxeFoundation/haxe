import python.lib.Time;

@:preCode("import sys")
@:coreApi
class Sys {

	public static function time():Float {
		return Time.time()/1000;
	}

	public static function exit(code:Int):Void {
		python.lib.Sys.exit(code);
	}

	public static function print(v:Dynamic):Void {
		var str = Std.string(v);
		untyped __python__('sys.stdout.buffer.write(("%s"%str).encode(\'utf-8\'))');
	}

	public static function println(v:Dynamic):Void {
		var str = Std.string(v);
		untyped __python__('sys.stdout.buffer.write(("%s\\n"%str).encode(\'utf-8\'))');
	}

	public static function args() : Array<String> {
		return [];
	}

	public static function getEnv( s : String ) : String {
		return "";
	}

	public static function putEnv( s : String, v : String ) : Void {

	}

	public static function environment() : haxe.ds.StringMap<String> {
		return new haxe.ds.StringMap();
	}

	public static function sleep( seconds : Float ) : Void {

	}

	public static function setTimeLocale( loc : String ) : Bool {
		return false;
	}

	public static function getCwd() : String {
		return "";
	}

	public static function setCwd( s : String ) : Void {
	}

	public static function systemName() : String {
		return "";
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int {
		return 0;
	}

	public static function cpuTime() : Float {
		return 0.0;
	}

	public static function executablePath() : String {
		return "";
	}

	public static function getChar( echo : Bool ) : Int {
		return 0;
	}

	public static function stdin() : haxe.io.Input {
		return null;
	}

	public static function stdout() : haxe.io.Output {
		return null;
	}

	public static function stderr() : haxe.io.Output {
		return null;
	}
}