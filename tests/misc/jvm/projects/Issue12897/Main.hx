// Anonymous-structure keys that aren't valid DEX SimpleNames (here: a key
// containing a newline + space). Without -D jvm.dex-compatible, genjvm emits
// a typed field literally named "a\n b" on the Anon class, which DEX rejects
// pre-040. With the define, the field is routed through DynamicObject's map
// so the jar is dex-clean. Behavior under Reflect / Json must be unchanged.
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
