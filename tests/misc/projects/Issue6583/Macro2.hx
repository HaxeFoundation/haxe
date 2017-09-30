class ChildClass extends Macro1.BaseClass {
	override public function f() {
		Sys.stderr().writeString("ChildClass.f");
	}
}

class Macro2 {
	static public macro function macro2() {
		Macro1.call(new ChildClass());
		return macro null;
	}
}