package unit.issues;

private abstract Integer(Int) from Int to Int {

	@:from static public function fromString(s:String):Integer {
		var n:Null<Int> = Std.parseInt(s);
		return cast(n, Integer);
	}

	@:to static public function toString(n:Integer):String {
		return ""+(n:Int);
	}
}

class Issue2957 extends Test {
	function test() {
		var n:Integer = "5";
	}
}