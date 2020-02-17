package unit.issues;

import unit.HelperMacros.typeString;

class Issue6836 extends Test {
	function test() {
		var v:Null<Int> = 1;
		var e1 = Std.random(2) == 1 ? 2 : v;
		eq(typeString(e1), "Null<Int>");
		var e2 = Std.random(2) == 1 ? v : 2;
		eq(typeString(e1), "Null<Int>");

		var v:Null<String> = "abc";
		var e1 = Std.random(2) == 1 ? "def" : v;
		eq(typeString(e1), "Null<String>");
		var e2 = Std.random(2) == 1 ? v : "def";
		eq(typeString(e1), "Null<String>");
	}
}
