package unit.issues;

import utest.Assert;

class Issue8513 extends unit.Test {
	function test() {
		var s = '';
		s = s + 1;
		s += 1;
		eq('11', s);
	}
}