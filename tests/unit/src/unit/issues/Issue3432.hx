package unit.issues;

private abstract A(String) {

	public function new(s) {
		this = s;
	}

	@:arrayAccess function getString(index:String) {
		return this + index;
	}

	@:arrayAccess function getInt(index:Int) {
		return this + Std.string(index);
	}

	@:arrayAccess inline function setString(index:String, value:String) {
		this += index + value;
	}

	@:arrayAccess inline function setInt(index:Int, value:String) {
		this += Std.string(index) + value;
	}
}

class Issue3432 extends Test {
	function test() {
		var a = new A("foo");
		eq("foobar", a["bar"]);
		eq("foo12", a[12]);
		a["bar"] = "baz";
		eq("foobarbazbaz", a["baz"]);
		eq("foobarbaz12", a[12]);
		a[12] = "foo";
		eq("foobarbaz12foofoo", a["foo"]);
		eq("foobarbaz12foo12", a[12]);
	}
}