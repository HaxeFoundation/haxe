package unit.issues;

import unit.issues.misc.Issue10259Macro;
import utest.Assert;

class Issue10259 extends Test {
	function test() {
		final maps = Issue10259Macro.getMaps();

		Assert.same([1 => 2], maps.intMap);
		Assert.same(["1" => 2], maps.stringMap);
		for (key => value in maps.objectMap) {
			eq(1, key.x);
			eq(2, value);
		}
	}
}
