package unit.issues;

class Issue3260 extends Test {

	function test() {
		eq("a,b", rest("a", "b"));
		eq("a,b,c", rest("a", "b", "c"));
		eq("a,b,c,d", rest("a", "b", "c", "d"));
		t(unit.TestType.typeError(rest()));
		t(unit.TestType.typeError(rest("a")));
		t(unit.TestType.typeError(rest("a", "b", 1)));
	}

	static function rest(first:String, second:String, rest:haxe.Rest<String>) {
		rest.unshift(second);
		rest.unshift(first);
		return rest.join(",");
	}
}