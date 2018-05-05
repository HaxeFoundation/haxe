package unit.issues;

import haxe.ds.Option;

private interface ITest {
	function foo():Int;
}

private class CTest implements ITest {
    public function new() {}
	public function foo() { return 1; }
}

class Issue6606 extends unit.Test {
	function test() {
		function callMe(o:Option<ITest>) { }
 		var o:Option<ITest> = Some(new CTest());
		callMe(Some(new CTest()));
		var c = Some(new CTest());
		t(unit.HelperMacros.typeError(callMe(c)));
	}
}