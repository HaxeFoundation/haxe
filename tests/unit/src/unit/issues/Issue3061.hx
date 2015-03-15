package unit.issues;

private enum MyEnum1 {
    A;
}

private enum MyEnum2 {
    A;
	B;
}

class Issue3061 extends Test {
	function test() {
		var e:Null<MyEnum1> = A;
		var r = foo(
			switch (e) {
				case A: 1;
			}
		);
		eq(1, r);

		var e2:Null<MyEnum2> = A;
		var r = foo(
			switch (e2) {
				case A: 1;
				case B: 2;
			}
		);
		eq(1, r);
	}

	static function foo( v : Int ) return v;
}