import python.lib.Time;

@:preCode("import sys")
class Sys {

	public static function time () {
		return Time.time()/1000;
	}

	public static function exit(code:Int) {
		python.lib.Sys.exit(code);
	}
	
	public static function print(str:String) {
		untyped __python__('sys.stdout.buffer.write(("%s"%str).encode(\'utf-8\'))');
	}
	
	public static function println(str:String) {
		untyped __python__('sys.stdout.buffer.write(("%s\\n"%str).encode(\'utf-8\'))');
	}
}