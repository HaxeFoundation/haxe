package unit.issues;

private abstract MyOpaciteInt(Int) {}

class Issue3110 extends Test {
	function test() {
		var o:List<MyOpaciteInt> = null;
		t(unit.TestType.typeError({
			var u:List<UInt> = o;
		}));
	}
}