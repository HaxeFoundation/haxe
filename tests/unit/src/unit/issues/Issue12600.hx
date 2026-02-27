package unit.issues;

@:nullSafety(Loose)
class Issue12600 extends Test {
	public var read:Int;
	public var write:Int;

	public function new() {
		super();
		// Test chained field assignment in constructor
		read = write = 0;
	}

	function test() {
		var c = new Issue12600();
		eq(0, c.read);
		eq(0, c.write);
	}
}

@:nullSafety(Loose)
class Issue12600MultiChain extends Test {
	public var a:Int;
	public var b:Int;
	public var c:Int;

	public function new() {
		super();
		// Test longer chain of assignments
		a = b = c = 42;
	}

	function test() {
		var obj = new Issue12600MultiChain();
		eq(42, obj.a);
		eq(42, obj.b);
		eq(42, obj.c);
	}
}

@:nullSafety(Strict)
class Issue12600Strict extends Test {
	public var x:Int;
	public var y:Int;

	public function new() {
		super();
		// Test that chained assignment also works in Strict mode
		x = y = 100;
	}

	function test() {
		var obj = new Issue12600Strict();
		eq(100, obj.x);
		eq(100, obj.y);
	}
}
