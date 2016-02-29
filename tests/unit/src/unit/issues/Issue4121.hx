package unit.issues;

import unit.issues.misc.Issue4121Macro.wrap;

class Issue4121 extends Test {
	function test() {
		eq("(@:test ((x) = (1)))", wrap(@:test x = 1));
		eq("(@:test (@:test2 ((x) = (@:test3 (1)))))", wrap(@:test @:test2 x = @:test3 1));
	}
}