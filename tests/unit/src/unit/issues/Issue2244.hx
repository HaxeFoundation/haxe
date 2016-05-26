package unit.issues;

import unit.Test;

class Issue2244 extends Test{
	function test() {
		var x = "foo";
		var y = '${ '${x}'}';
		eq("foo", y);
	}
}