package unit.issues;

private abstract Optional<T>(haxe.ds.Option<T>) from haxe.ds.Option<T> {}

class Issue3528 extends Test {
    function test() {
		var x = test1(Some(4));

		test2(Some(4));
		switch test2(x) {
			case Some(v):
			case None:
		}
		noAssert();
    }

	static function test1(v:haxe.ds.Option<Int>) {
		return v;
	}

	static function test2(v:Optional<Int>) {
		return v;
	}
}