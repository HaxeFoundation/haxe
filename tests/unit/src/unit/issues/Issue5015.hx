package unit.issues;

using haxe.Json;

class Issue5015 extends Test {
	function test() {
		eq('{}'.parse().stringify(), '{}');
	}
}