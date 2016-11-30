package unit.issues;

class Issue5028 extends Test {
	var data:Data = { f: -1 };

	function test() {
		t(Math.random() > data.f);
	}
}

typedef Data = {
    ?f:Float
}