package unit.issues;

class Issue3894 extends Test {
	function test() {
		#if (flash || cs || java || cpp)
		// this doesn't actually error until post-processing so we cannot test it like this
		//t(unit.TestType.typeError((null : haxe.Int64)));
		#end
	}
}