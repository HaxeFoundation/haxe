package unit.issues;

import unit.HelperMacros.typeString;

class Issue9476 extends unit.Test {
	static var a:Null<Int> = 42;
	@:nullSafety
	function test() {
		var b = a!;
		eq("Int", typeString(b));
		eq(42, b);
	}
}
