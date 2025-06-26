package unit.issues;

private enum Foo {
	FooRec(foo:Foo);
}

class Issue11429 extends Test {
	function test() {
		blowUp([]);
		utest.Assert.pass();
	}

	public static function blowUp(arr:Array<Foo>) {
		var qp = FooRec(arr.pop());
		while (arr.length > 0) {
			qp = FooRec(qp);
		}
	}
}
