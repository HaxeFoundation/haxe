class BaseClass {
	public function new() { }
	public function f() {
		Sys.stderr().writeString("BaseClass.f");
	}
}

class Macro1 {
	static public macro function macro1() {
		return macro null;
	}

	static public function call(ref:BaseClass) {
		ref.f();
	}
}