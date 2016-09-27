package unit.issues;

import unit.issues.misc.Issue4121Macro.wrap;

private typedef P2 = {
	var hidden(null, null):Int;
}

class Issue4121 extends Test {
	function test() {
		eq("(@:test ((x) = (1)))", wrap(@:test x = 1));
		eq("(@:test (@:test2 ((x) = (@:test3 (1)))))", wrap(@:test @:test2 x = @:test3 1));

		var p2:P2 = { hidden: 12 };
		t(unit.HelperMacros.typeError(p2.hidden));
		eq(12, @:privateAccess p2.hidden);
		t(unit.HelperMacros.typeError(p2.hidden = 13));
		@:privateAccess p2.hidden = 13;
		eq(13, @:privateAccess p2.hidden);
	}
}