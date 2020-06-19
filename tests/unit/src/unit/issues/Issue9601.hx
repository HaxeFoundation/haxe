package unit.issues;

class Issue9601 extends Test {
	#if java
	public function test() {
		utest.Assert.same(["", "Test"], ~/^/g.split("Test"));
		utest.Assert.same(["Test", ""], ~/$/g.split("Test"));
		utest.Assert.same(["", "Test", ""], ~/\b/g.split("Test"));

		utest.Assert.same(["", "Test"], ~/^/.split("Test"));
		utest.Assert.same(["Test", ""], ~/$/.split("Test"));
		utest.Assert.same(["", "Test"], ~/\b/.split("Test"));
	}
	#end
}

