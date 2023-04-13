package unit.issues;

import haxe.Rest;

class Issue10761 extends Test {
	function rest(args:Rest<Int>) {
		return args;
	}

	#if !erase_generics
	function test() {
		aeq([0, 1], rest(0, 1)); // works
		aeq([0, 1], rest(...[0, 1])); // works
		aeq([0, 1], rest(...[for (i in 0...2) i])); // errors
	}
	#end
}
