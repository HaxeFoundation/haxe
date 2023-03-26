package unit.issues;

class Issue7736 extends Test {
	#if !cs
	@:nullSafety
	function test() {
		var found = null;
		for (i in 0...10) {
			if (i == 5) {
				found = i;
				break;
			}
		}
		unit.HelperMacros.typedAs(found, (null : Null<Int>));
		if (found != null) {
			var x:Int = found;
			eq(5, x);
		}
	}
	#end
}
