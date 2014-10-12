package unit.issues;

private class MyTestClass {
	public function new() { }
}

class Issue2907 extends Test {

	@:generic function catchGeneric<T>(t:T) {
		try {
			throw "foo";
		} catch(e:T) {
			return e;
		} catch(e:Dynamic) {
			return null;
		}
	}

	function test<T>() {
		t(unit.TestType.typeError(
			try { }
			catch(e:T) { }
		));
		eq("foo", catchGeneric("foo"));
		eq(null, catchGeneric(new MyTestClass()));
	}
}