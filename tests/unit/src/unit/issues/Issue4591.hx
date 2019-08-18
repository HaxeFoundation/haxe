package unit.issues;

class Issue4591 extends Test {
	public function test() {
		var x:C<C<String>> = null;
		noAssert();
	}
}

abstract C<T>(T) from T to T { }