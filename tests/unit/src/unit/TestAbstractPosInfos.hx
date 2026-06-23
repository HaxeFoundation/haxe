package unit;

// A trailing optional haxe.PosInfos is auto-filled (and forwards the call site)
// on every abstract cast / operator-overload form, not just on plain functions.
class TestAbstractPosInfos extends Test {
	function testFromStatic() {
		var f:FromS = 7;
		eq("7@testFromStatic", f.get());
	}

	function testToMember() {
		var b = new ToMember("a");
		var s:String = b;
		eq("a@testToMember", s);
	}

	function testToStatic() {
		var t = new ToStatic("a");
		var arr:Array<String> = t;
		eq("a", arr[0]);
		eq("testToStatic", arr[1]);
	}

	function testArrayAccess() {
		var a = new Arr();
		a[0] = "x"; // set: stores "x/testArrayAccess"
		eq("x/testArrayAccess|testArrayAccess", a[0]); // get: appends
	}

	function testResolve() {
		var d = new Dyn();
		d.foo = "bar"; // write resolve: stores "bar/testResolve"
		eq("bar/testResolve|testResolve", d.foo); // read resolve: appends
		eq("?|testResolve", d.missing);
	}

	function testCallable() {
		var fn = new Fn("f");
		eq("f(42)@testCallable", fn(42));
	}
}

private abstract FromS(String) {
	public inline function new(s) this = s;
	public inline function get():String return this;
	@:from static function fromInt(i:Int, ?pos:haxe.PosInfos):FromS return new FromS(i + "@" + pos.methodName);
}

private abstract ToMember(String) {
	public inline function new(s) this = s;
	@:to function toStr(?pos:haxe.PosInfos):String return this + "@" + pos.methodName;
}

private abstract ToStatic(String) {
	public inline function new(s) this = s;
	// static @:to: first argument is the underlying type
	@:to static function toArr(u:String, ?pos:haxe.PosInfos):Array<String> return [u, pos.methodName];
}

private abstract Arr(Array<String>) {
	public inline function new() this = [];
	@:arrayAccess function get(i:Int, ?pos:haxe.PosInfos):String return this[i] + "|" + pos.methodName;
	@:arrayAccess function set(i:Int, v:String, ?pos:haxe.PosInfos):String {
		this[i] = v + "/" + pos.methodName;
		return this[i];
	}
}

private abstract Dyn(Map<String, String>) {
	public inline function new() this = new Map();
	@:op(a.b) function field(name:String, ?pos:haxe.PosInfos):String
		return (this.exists(name) ? this[name] : "?") + "|" + pos.methodName;
	@:op(a.b) function setField(name:String, value:String, ?pos:haxe.PosInfos):String {
		this[name] = value + "/" + pos.methodName;
		return value;
	}
}

private abstract Fn(String) {
	public inline function new(s) this = s;
	@:op(a()) function call(x:Int, ?pos:haxe.PosInfos):String return this + "(" + x + ")@" + pos.methodName;
}
