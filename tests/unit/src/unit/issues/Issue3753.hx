package unit.issues;

private abstract A(Map<String, String>) from Map<String, String> {
	@:resolve
	function resolve(name:String) {
		return this[name];
	}
}

private abstract B(Map<String, String>) from Map<String, String> {
	@:resolve
	static function resolve(lhs:B, name:String) {
		return lhs.get()[name];
	}

	function get() return this;
}

private abstract C(Map<String, String>) from Map<String, String> {
	@:resolve
	macro function resolve(ethis:haxe.macro.Expr, name:String) {
		var s = switch (name) {
			case "foo": "bar";
			case "bar": "baz";
			case _: null;
		}
		return macro $v{s};
	}
}

private abstract D(Map<String, String>) from Map<String, String> {
	@:resolve
	macro static function resolve(ethis:haxe.macro.Expr, name:String) {
		var s = switch (name) {
			case "foo": "bar";
			case "bar": "baz";
			case _: null;
		}
		return macro $v{s};
	}
}

class Issue3753 extends Test {
	function test() {
		var a:A = ["foo" => "bar", "bar" => "baz"];
		eq("bar", a.foo);
		eq("baz", a.bar);

		var a:B = ["foo" => "bar", "bar" => "baz"];
		eq("bar", a.foo);
		eq("baz", a.bar);

		var a:C = ["foo" => "bar", "bar" => "baz"];
		eq("bar", a.foo);
		eq("baz", a.bar);

		var a:D = ["foo" => "bar", "bar" => "baz"];
		eq("bar", a.foo);
		eq("baz", a.bar);
	}
}