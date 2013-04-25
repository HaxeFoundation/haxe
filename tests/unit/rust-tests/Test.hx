import rust.StdTypes;
class Test {
	public var severity:Int = 215692;
	function new() {

	}
	public static function main() {
		var a:Null<Int>, b:Single, c:Int8, d:Test;
		a = null;
		if(a == null)
			Sys.println("A IS AN NULL");
		d = new Test();
		Sys.println(Std.string(d.severity));
	}
}