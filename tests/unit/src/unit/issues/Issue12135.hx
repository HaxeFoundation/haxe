package unit.issues;

enum Foo12135 {
	Foo:Null<Foo12135>;
}

class Issue12135 extends Test {
	function test() {
		eq(Foo12135.Foo, Foo12135.Foo);
	}
}
