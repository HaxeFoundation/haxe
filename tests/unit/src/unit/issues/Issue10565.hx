package unit.issues;

private abstract MapProxy<K, V>(Dynamic) {
	@:from static function fromMap<K, V>(map:Map<K, V>):MapProxy<K, V> {
		return null;
	}
}

class Issue10565 extends Test {
	function test() {
		var m:MapProxy<String, Int> = new Map();
		utest.Assert.pass();
	}
}
