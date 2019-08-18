package unit.issues;

import haxe.format.JsonParser;

class Issue5691 extends unit.Test {
	function test() {
		var jsons =  ['trueeee', 'falseah', 'nullol', '123asd', '{"a" : 1} trail'];
		for (j in jsons) {
			try {
				JsonParser.parse(j);
				t(false);
			} catch(e:Dynamic) {
				t(true);
			}
		}
		try {
			JsonParser.parse("1e2");
			t(true);
		}
		catch (e:Dynamic) {
			t(false);
		}
		try {
			JsonParser.parse("1e2.5");
			t(false);
		}
		catch(e:Dynamic) {
			t(true);
		}
	}
}
