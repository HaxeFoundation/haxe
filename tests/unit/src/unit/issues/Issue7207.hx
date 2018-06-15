package unit.issues;

class Issue7207 extends unit.Test {
	function test() {
		eq("Haxe is great! 1", 'Haxe is great! ${/*13*/1}');
	}
}