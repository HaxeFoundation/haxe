package unit.issues;

private abstract Either<T1,T2>(Dynamic) from T1 from T2 to T1 to T2 {}

class Issue3364 extends Test {

	function test() {
		eq("hello", func("hello"));
	}

	static function func(v:Either<String,Array<Int>>) {
		return v;
	}
}