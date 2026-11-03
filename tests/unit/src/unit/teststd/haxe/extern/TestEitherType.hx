package unit.teststd.haxe.extern;

class TestEitherType extends unit.Test {
	public function test() {
		var e:haxe.extern.EitherType<Int,String> = "string";
		var s:String = e;
		eq(s, "string");
		e = 1;
		var i:Int = e;
		eq(i, 1);
		t(HelperMacros.typeError(e = false));
		t(HelperMacros.typeError(e = 1.5));

	}
}
