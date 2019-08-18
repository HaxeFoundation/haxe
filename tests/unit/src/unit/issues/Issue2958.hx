package unit.issues;

import unit.HelperMacros.typeString;

private typedef Asset<@:const T> = String;

class Issue2958 extends Test {
	function test() {
		eq(
		   typeString((null : Asset<["test", 1]>)),
		   "unit.issues._Issue2958.Asset<[\"test\", 1]>"
		);
	}
}