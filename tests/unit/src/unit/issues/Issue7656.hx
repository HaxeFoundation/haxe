package unit.issues;

private class C<T> {
	public function new() {}
}

private abstract A<T>(Dynamic)
	from C<T>
	from T
{}

class Issue7656 extends unit.Test {

	function test() {
		var v = sf(new C<Int>());
		HelperMacros.typedAs(v, (null : C<Int>));
	}

	static function sf<T>(v:A<T>):C<T> return null;
}