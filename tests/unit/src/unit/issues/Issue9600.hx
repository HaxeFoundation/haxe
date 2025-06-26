package unit.issues;

class Issue9600 extends Test {
	function test() {
		var error = unit.issues.misc.Issue9600Macro.contextTypeofError();
		eq('{ b : Bool, a : Int } has extra field b', error);
	}
}
