package unit.issues;

import unit.HelperMacros.typeString;

private typedef A = {
    @:optional var value:Null<String>;
}

class Issue6896 extends Test {
	function test() {
		var a:A = {};
		eq(typeString(a.value), "Null<String>");
	}
}
