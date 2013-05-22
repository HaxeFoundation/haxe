import rust.StdTypes;
class Test {
	public var val:Int;
	public var tok:String;
	function new(t:String) {
		val = 40;
		tok = t;
	}
	public static function main() {
		Sys.println(Std.string(new Test("Hallo")));
		Sys.println("Hello, world!");
	}
}