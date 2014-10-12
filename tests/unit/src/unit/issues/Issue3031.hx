package unit.issues;

private class X {
	public var s:String;
	public function new(s:String) {
		this.s = s;
	}
}

class Issue3031 extends Test {
	function testArty() {
		var a = "";
		switch (new StringBuf()) {
			case null: a = "null";
			case notNull: a = "not null";
		}
		eq("not null", a);
	}

	function testHeinz() {
		function doSwitch(x1:Null<X>, x2:Null<X>) {
			return switch [x1,x2] {
				case [null,null]: "";
				case [a,null]: a.s;
				case [null,b]: b.s;
				case [a,b]: a.s + b.s;
			}
		}
		eq("", doSwitch(null, null));
		eq("foo", doSwitch(new X("foo"), null));
		eq("bar", doSwitch(null, new X("bar")));
		eq("foobar", doSwitch(new X("foo"), new X("bar")));
	}
}