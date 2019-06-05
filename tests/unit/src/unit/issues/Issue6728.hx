package unit.issues;

class Issue6728 extends unit.Test {
	@:analyzer(ignore)
	function test() {
		stringMap();
		intMap();
		objectMap();
		enumValueMap();

		noAssert();
	}

	function stringMap() {
		var map = new Map<String, Dynamic>();
		map[''] = map;
		return map.toString();
	}

	function intMap() {
		var map = new Map<Int, Dynamic>();
		map[1] = map;
		return map.toString();
	}

	function objectMap() {
		var map = new Map<{}, Dynamic>();
		map[{}] = map;
		return map.toString();
	}

	function enumValueMap() {
		var map = new Map<Dummy, Dynamic>();
		map[One] = map;
		return map.toString();
	}
}

private enum Dummy {
	One;
}
