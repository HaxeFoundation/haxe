package unit.issues;

class Issue11068 extends Test {
	function test() {
		final data:Dynamic = ({} : Dynamic);
		data.id = 0;
		use(data);
		utest.Assert.pass();
	}

	static function use(v:Any) {}
}
