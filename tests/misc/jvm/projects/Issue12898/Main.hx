// Anonymous-structure keys that aren't valid DEX SimpleNames (here: a key
// containing a newline + space). genjvm routes such fields through
// DynamicObject's `_hx_fields` map instead of emitting a typed JVM field
// literally named "a\n b" (which DEX rejects pre-040). Behavior under
// Reflect / Json must be unchanged.
class Main {
	static public function main() {
		final a:Dynamic = {"a\n b": 1, normal: 2};

		assertEq(1, Reflect.field(a, "a\n b"));
		assertEq(2, Reflect.field(a, "normal"));

		final fields = Reflect.fields(a);
		fields.sort(Reflect.compare);
		assertEq("[a\n b,normal]", "[" + fields.join(",") + "]");

		Reflect.setField(a, "a\n b", 42);
		assertEq(42, Reflect.field(a, "a\n b"));
	}

	static function assertEq<T>(expected:T, actual:T) {
		if (expected != actual) {
			Sys.println('FAIL: expected $expected, got $actual');
			Sys.exit(1);
		}
	}
}
