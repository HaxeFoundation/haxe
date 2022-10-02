package unit.issues;

private abstract A(String) {
	var selected(get, set):Dynamic;

	public function get_selected() {
		return null;
	}

	public function set_selected(v) {
		return null;
	}

	public function new() {
		this = "";
	}
}

private class C {
	var selected(get, set):Dynamic;

	public function get_selected() {
		return null;
	}

	public function set_selected(v) {
		return null;
	}

	public function new() {}
}

class Issue10568 extends Test {
	static var selected(get, set):Dynamic;

	static public function get_selected() {
		return null;
	}

	static public function set_selected(v) {
		return null;
	}

	function test() {
		HelperMacros.typedAs(get_selected, (null : () -> Dynamic));
		HelperMacros.typedAs(set_selected, (null : (v:Dynamic) -> Dynamic));

		var c = new C();
		HelperMacros.typedAs(c.get_selected, (null : () -> Dynamic));
		HelperMacros.typedAs(c.set_selected, (null : (v:Dynamic) -> Dynamic));

		var a = new A();
		HelperMacros.typedAs(a.get_selected, (null : () -> Dynamic));
		HelperMacros.typedAs(a.set_selected, (null : (v:Dynamic) -> Dynamic));
	}
}
