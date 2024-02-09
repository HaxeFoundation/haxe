package unit.issues;

class Issue9641 extends unit.Test {
	function test() {
		try {
			throw new MyException<Int>();
		} catch(e:MyException<Any>) {
			noAssert();
		}
	}
}

private class MyException<T> {
	public function new() {}
}
