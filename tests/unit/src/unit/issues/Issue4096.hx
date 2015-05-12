package unit.issues;

using Lambda;

class Issue4096 extends Test {
	function test() {
		eq(1, Xml.parse("<?xml ?>").count());
		eq(1, Xml.parse("<!-- comment -->").count());
		eq(1, Xml.parse("<!DOCTYPE >").count());
	}
}