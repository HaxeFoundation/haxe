package unit.issues;

@:structInit
private class OptionalProperty {
	public var normalVar:Int = 5;
	@:optional
	public var prop(get, set):Int;

	function get_prop() {
		return 10;
	}

	function set_prop(v) {
		normalVar = 10;
		return v;
	}
}

@:structInit
private class Property {
	public var normalVar:Int = 5;
	public var prop(get, set):Int;

	function get_prop() {
		return 10;
	}

	function set_prop(v) {
		normalVar = 10;
		return v;
	}
}

class Issue10652 extends Test {
	function test() {
		var empty:OptionalProperty = {};
		eq(10, empty.prop);
		eq(5, empty.normalVar);

		var empty:Property = {};
		eq(10, empty.prop);
		eq(5, empty.normalVar);
	}
}