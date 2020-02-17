package unit.issues;

private enum MyEnum {
	MyEnumConstructor(t:String -> MyEnum);
}

private abstract A(String -> MyEnum) to String -> MyEnum {
	public function new(f) {
		this = f;
	}
}

class Issue6609 extends unit.Test {
	function test() {
		MyEnumConstructor(new A(null));
		noAssert();
	}
}