package unit.issues;

import unit.issues.misc.Issue10259Macro;
import utest.Assert;

class Issue10259 extends Test {
	function test() {
		final maps = Issue10259Macro.getMaps();

		Assert.same([1 => 2], maps.intMap);
		Assert.same(["1" => 2], maps.stringMap);
		// Not sure if this is specified to actually work x)
		eq(1, maps.objectMap.keys().next().x);
		eq(2, maps.objectMap.iterator().next());
	}
}
