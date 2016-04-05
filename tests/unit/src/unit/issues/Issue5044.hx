package unit.issues;

class Issue5044 extends Test {
	function test() {
		var i = Int(5);
		eq(5, switch (i) {
			case Int(v): v;
		});
	}
}

enum TokenDef {
	Int(v:Int);
}