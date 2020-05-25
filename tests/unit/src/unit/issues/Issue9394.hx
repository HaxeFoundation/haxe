package unit.issues;

import misc.Issue9394Class;

class Issue9394 extends unit.Test {
	@:analyzer(no_local_dce)
	function test() {
		var misc = Std.random(10);
		Issue9394Class.test();
		eq(misc, misc);
	}
}