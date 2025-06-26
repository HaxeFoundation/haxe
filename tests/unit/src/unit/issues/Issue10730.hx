package unit.issues;

import haxe.extern.EitherType;

private abstract A(String) {}
private typedef ET = EitherType<DocumentFilter, A>;

private typedef DocumentFilter = {
	final ?language:String;
	final ?scheme:String;
}

class Issue10730 extends Test {
	function testSimn() {
		final et:ET = {language: "haxe"};
		utest.Assert.pass();
	}
}
