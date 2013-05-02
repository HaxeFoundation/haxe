import rust.StdTypes;
class Test {
	var value:Null<Int>;
	function new() {
		value = null;
	}
	static function assert(v:Bool, ?msg:String):Void {
		if(!v)
			throw msg;
	}
	public static function main() {
		var c = new Test();
		c.value = 67 % 2;
		c.value = Std.int(c.value / 0.2);
		assert(c.value == 5);
	}
}