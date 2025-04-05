package unit.issues;

class Issue8984 extends Test {
	function test() {
		var m = [1 => 'hello'];
		iter(m);
		utest.Assert.pass();
	}

	static function iter<T:Iterable<String>>(m:T) {}
}