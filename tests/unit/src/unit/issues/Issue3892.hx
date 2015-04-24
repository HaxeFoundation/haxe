package unit.issues;

abstract MyBool(Bool) {
    public inline function new(x:Bool) this = x;
    public inline function toString():String return "asString: " + this;
}

class Issue3892 extends Test {
	function test() {
		var z = new MyBool(true);
		eq("asString: true", z.toString());
	}
}