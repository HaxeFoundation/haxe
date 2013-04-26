import rust.StdTypes;
class Test {
	public var severity:Int = 215692;
	function new() {

	}
	public static function main() {
		var func:Int -> Void;
		func = function(n) Sys.println(Std.string(n + 1));
		func(24);
	}
}