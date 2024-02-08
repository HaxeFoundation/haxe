package unit.issues;

private enum E {
    A;
    B(n:Int);
}

class Issue5011 extends unit.Test {
    var e:E;
	var n(get,never):Null<Int>;

	function get_n() return switch (e) {
        case A: null;
        case B(n): n;
	}

	function test() {
		e = B(12);
		eq(12, get_n());
	}
}