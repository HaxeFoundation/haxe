package unit.issues;

private abstract Foo({bar:Int}) {
	public function new(bar:Int) {
		this = {bar: bar};
	}

	public function getBar() {
		return this.bar;
	}
}

class Issue10221 extends Test {
	function test() {
		final map:Map<Foo, Int> = new Map<Foo, Int>();
		final foo = new Foo(1);
		map.set(foo, 2);
		eq(2, map.get(foo));
		eq(1, [for (k in map.keys()) k][0].getBar());
	}
}