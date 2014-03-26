import python.lib.Time;

@:preCode("import sys")
class Sys {

	public static function time () {
		return Time.time()/1000;
	}

	public static function exit(code:Int) {
		python.lib.Sys.exit(code);
	}
}