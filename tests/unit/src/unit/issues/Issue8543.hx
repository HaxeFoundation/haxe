package unit.issues;

import unit.issues.misc.issue8543.hx.Sample;

class Issue8543 extends unit.Test {
	function test() {
		eq('hello', Sample.test());
	}
}