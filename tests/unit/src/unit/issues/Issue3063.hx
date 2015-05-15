package unit.issues;

@:enum
private abstract AString(String) to String {
    var CTOR1 = "foo";
}

@:enum
private abstract AInt(Int) to Int {
    var CTOR2 = 12;
}

class Issue3063 extends Test {
	function test() {
		var a = new Map();
		a.set(CTOR1, 1);
		eq(1, a.get(CTOR1));
		t((a is haxe.ds.StringMap));

		var b = new Map();
		b.set(CTOR2, 1);
		eq(1, b.get(CTOR2));
		t((b is haxe.ds.IntMap));
	}
}