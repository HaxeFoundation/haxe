package unit.issues;

class Issue9026 extends unit.Test {
	function test() {
		var key = {};
		var map: Map<{}, Int> = new Map();
		map[key] = 10;
		map[key] = 20;
		eq(20, map.get(key));
		eq(1, Lambda.count(map));
	}
}