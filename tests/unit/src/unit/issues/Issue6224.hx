package unit.issues;

private abstract S(String) from String {
	inline function asString() return this;
	@:op(a in b) inline static function contains(a:S, b:S) return b.asString().indexOf(a.asString()) != -1;
}

class Issue6224 extends unit.Test {
	function test() {
		var s:S = "hello";
		t("hell" in s);
	}
}
