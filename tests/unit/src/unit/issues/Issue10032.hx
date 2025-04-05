package unit.issues;

private enum Foo {
	A;
	B;
}

class Issue10032 extends Test {
	static var a:Foo = A;

	static function onChange(v:Foo)
		switch a = v {
			case A: // Commenting out this line fixes the issue.
			case _:
		}

	function test() {
		onChange(B);
		t(a == B);
	}
}
