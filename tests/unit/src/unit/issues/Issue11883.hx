package unit.issues;

import unit.Test;

class Issue11883 extends Test {
	function test() {
		eq(Xml.parse("<foo>xx</foo> bar&lt;").toString(), "<foo>xx</foo> bar&lt;");
	}
}
