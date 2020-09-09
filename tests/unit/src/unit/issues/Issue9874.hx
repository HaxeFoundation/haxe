package unit.issues;

import haxe.Constraints.IMap;

@:forward abstract ArrayMap<K, V>(IMap<K, Array<V>>) from IMap<K, Array<V>> to IMap<K, Array<V>> {}

class Issue9874 extends unit.Test {
	function test() {
		var m:ArrayMap<String, String> = new Map();
		var m2:ArrayMap<String, String> = new Map<String, Array<String>>();
		var m3:ArrayMap<String, String> = new haxe.ds.StringMap<Array<String>>();
		utest.Assert.pass();
	}
}
