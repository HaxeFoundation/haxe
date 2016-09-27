package unit.issues;

private enum E {
	E1<T:String>(v:T);
}

private abstract Ab(String) to String {
	public function new() {
		this = "foo";
	}
}

class Issue3781 extends Test {

	function test() {
		call(new Ab());
	}

	static function call<T:Ab>(v:T) {
		E1(v);
	}
}