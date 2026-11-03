package unit.issues;

import haxe.Json;

class Issue12221 extends unit.Test {
	function test() {
		var str = Json.stringify({a: "👍"});
		t(str == '{"a":"👍"}' || str == '{"a":"\\ud83d\\udc4d"}');
		var parsed:Dynamic = Json.parse(str);
		eq("👍", parsed.a);

		var s = "a😀\"é👽b";
		eq(s, Json.parse(Json.stringify(s)));
	}
}
