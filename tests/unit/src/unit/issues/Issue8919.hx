package unit.issues;

import unit.issues.misc.Issue8919Macro;

private typedef TestTd = {
	var s:String;
}

@:forward
private abstract TestAb(TestTd) from TestTd {}

class Issue8919 extends Test {
	function test() {
		var a:TestAb = {s:"Hi!"};
		Issue8919Macro.check(a.s);
	}
}