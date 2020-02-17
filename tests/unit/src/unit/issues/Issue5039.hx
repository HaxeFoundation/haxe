package unit.issues;

private class C {
	@:isVar public var prop(get,never):Int = 42;
	function get_prop():Int throw "FAIL prop";

	public function new() {}
}

class Issue5039 extends Test {
	var getterCalled = false;
	var setterCalled = false;

	@:isVar var x(get,set):Int = 4;
	function get_x():Int {
		getterCalled = true;
		return x;
	}

	function set_x(value:Int):Int {
		setterCalled = true;
		return x = value;
	}

	@:isVar public var c(get,never):C = new C();
	function get_c():C throw "FAIL c";

	function test() {
		@:bypassAccessor x = x + 1;
		t(getterCalled);
		f(setterCalled);

		getterCalled = false;

		x = @:bypassAccessor x + 1;
		f(getterCalled);
		t(setterCalled);

		setterCalled = false;

		@:bypassAccessor x = @:bypassAccessor x + 1;
		f(getterCalled);
		f(setterCalled);

		t(Std.isOfType(@:bypassAccessor c, C));
		eq(42, @:bypassAccessor (@:bypassAccessor c).prop);
		eq(42, @:bypassAccessor @:bypassAccessor c.prop);
	}
}
