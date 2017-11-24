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
	}
}
