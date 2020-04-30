package unit.issues;

class Issue9057 extends unit.Test {
	function test() {
		var foo:Foo = 0;

		switch foo {
			case v if(v):
				noAssert();
				return;
			case v:
		}
		assert();
	}
}

private abstract Foo(Int) from Int to Int {
	@:to
	public function toBool():Bool
		return true;
}