package unit.issues;

using unit.issues.misc.Issue2152Class;

private abstract MyInt(Int) {
	public inline function new (x:Int) this = x;
	@:to inline function toString ():String return "asString: " + this;
}

class Issue2152 extends Test {
	function test() {
		var z = new MyInt(1);
		eq("asString: 1", z.passString());
	}
}