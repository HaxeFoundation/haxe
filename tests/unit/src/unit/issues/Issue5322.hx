package unit.issues;

import unit.HelperMacros.typeErrorText;
import unit.HelperMacros.typedAs;

private abstract A(String) from String {}

class Issue5322 extends unit.Test {
	function test() {
		// unifies!
		var a:Any = 1;

		// no fields!
		eq("Any has no field f", typeErrorText(a.f));

		// no array access!
		eq("Array access is not allowed on Any", typeErrorText(a[0]));

		// no comparison!
		eq("Cannot compare Any and Int", typeErrorText(a > 1));

		// kept as the return type!
		typedAs(something(), (1:Any));

		// promotes to another type successfully!
		eq("HELLO", (something() : String).toUpperCase());

		// can be used for array of mixed types!
		var a:Array<Any> = [1,false,"hey",{}];
		eq("hey", (a[2] : String));

		eq("hello", ((("hello" : A) : Any) : String));
	}

	function something():Any return "hello";
}
