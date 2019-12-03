package unit.issues;

#if flash
private class PropClassChild extends PropClass {
	override function get_x():Int {
		return super.get_x() + 1;
	}

	override function set_x(value:Int) {
		return super.set_x(value + 1);
	}
}

private class PropIfaceImpl implements PropIface {
	var _x = 42;
	@:flash.property
	public var x(get,set):Int;
	function get_x():Int return _x;
	function set_x(v:Int):Int return _x = v;
	public function new() {}
}

private interface HaxePropIface {
	var x(get,set):Int;
}

private class HaxePropIfaceImpl extends PropClass implements HaxePropIface {}
#end

class Issue8241 extends unit.Test {
	#if flash
	function test() {
		var p = new PropClass();
		eq(42, p.x);
		eq(15, p.x = 15);
		eq(15, p.x);

		var p = new PropClassChild();
		eq(43, p.x);
		eq(17, p.x = 16);
		eq(18, p.x);

		var p:PropClass = new PropClassChild();
		eq(43, p.x);
		eq(16, p.x = 16); // this should actually return 17 like in the example above, but that's not how flash properties work,
		                  // so technically we should do something about it, but this only covers a really insane when the setter returns a different value
		eq(18, p.x);

		var p = new PropIfaceImpl();
		eq(42, p.x);
		eq(15, p.x = 15);
		eq(15, p.x);

		var p:PropIface = new PropIfaceImpl();
		eq(42, p.x);
		eq(15, p.x = 15);
		eq(15, p.x);

		var p = new HaxePropIfaceImpl();
		eq(42, p.x);
		eq(15, p.x = 15);
		eq(15, p.x);

		var p:HaxePropIface = new HaxePropIfaceImpl();
		eq(42, p.x);
		eq(15, p.x = 15);
		eq(15, p.x);
	}
	#end
}
