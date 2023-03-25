package unit.issues;

class Issue3297 extends Test {
	function test() {
		var json = haxe.Json.parse('{ "array": [1, 2, 3] }');
		eq(json.array.length, 3);
		eq(json.array[0], 1);
	}
}
