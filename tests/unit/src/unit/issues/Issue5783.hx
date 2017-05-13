package unit.issues;

private abstract I(Int) from Int to Int {
	@:from
	public static function fromString(v:String):I return Std.parseInt(v);
}

class Issue5783 extends unit.Test {
	function test() {
		function add(i:I) return i + 1;
		var f = add.bind('1');
	}
}