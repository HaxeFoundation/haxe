package unit.issues;

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

	function test() {
		@:bypassAccessor x = x + 1;
		f(getterCalled);
		f(setterCalled);

		x = @:bypassAccessor x + 1;
		f(getterCalled);
		t(setterCalled);

		setterCalled = false;
		@:bypassAccessor x = @:noBypassAccessor x + 1;
		t(getterCalled);
		f(setterCalled);
	}
}
