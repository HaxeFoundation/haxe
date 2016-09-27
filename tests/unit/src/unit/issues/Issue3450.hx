package unit.issues;

@:generic class X<@:const T, @:const S> {
	public function new() { }

	public function getString() {
		return T + " " + S;
	}
}

class Issue3450 extends Test {
	function test() {
		var c = new X<'foo', 12>();
		eq("foo 12", c.getString());
	}
}