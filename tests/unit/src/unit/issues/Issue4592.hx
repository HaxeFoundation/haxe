package unit.issues;

import haxe.Json;

class Issue4592 extends unit.Test {
	function test() {
		var json =  'hello":"world"';
		try {
			Json.parse(json);
			t(false);
		} catch(e:Dynamic) {
			t(true);
		}
	}
}