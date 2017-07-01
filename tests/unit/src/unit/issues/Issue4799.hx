package unit.issues;

import unit.HelperMacros.typeErrorText;
import unit.HelperMacros.typeString;

class Issue4799 extends Test {
	function test() {
		var f : arg1:Int->?arg2:String->Float->Void;
		eq(typeString(f), "arg1 : Int -> ?arg2 : String -> Float -> Void");

		eq(typeErrorText((null : arg:Int)), "Named type not allowed here");

		// TODO: maybe we could actually allow this?
		eq(typeErrorText((null : Int->returnValue:Int)), "Named type not allowed here");
	}
}
