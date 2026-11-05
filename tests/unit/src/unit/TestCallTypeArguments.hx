package unit;

private class Element {
	public function new() {}
}

private class InputElement extends Element {
	public var value = "input";

	public function new() {
		super();
	}
}

private class Document {
	public function new() {}

	public function find<T:Element>(id:String):Null<T> {
		return cast new InputElement();
	}
}

class TestCallTypeArguments extends Test {
	static function pair<A, B>(a:A, b:B):{a:A, b:B} {
		return {a: a, b: b};
	}

	function testInstanceCall() {
		var input = new Document().find.<InputElement>("name");
		eq("input", input.value);
		t(HelperMacros.typeString(new Document().find.<InputElement>("name")) == "Null<unit._TestCallTypeArguments.InputElement>");
	}

	function testStaticCall() {
		var p = pair.<Float, Array<Array<Int>>>(1, [[2]]);
		eq("Float", HelperMacros.typeString(p.a));
		eq(2, p.b[0][0]);
	}

	function testStdCall() {
		eq("1,2", [1, 2].map.<String>(i -> Std.string(i)).join(","));
	}

	function testErrors() {
		t(HelperMacros.typeError(pair.<Int>(1, 2)));
		t(HelperMacros.typeError(pair.<String, Int>(1, 2)));
		t(HelperMacros.typeError(new Document().find.<String>("name")));
		t(HelperMacros.typeError(var f = pair.<Int, Int>));
	}
}
