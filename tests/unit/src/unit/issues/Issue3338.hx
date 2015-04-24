package unit.issues;

class Issue3338 extends Test {
	function test() {
        var e:TestEnum = null;
		
		match(Val1, 0);
		match(null, 1);
		match(Val2, 2);
	}
	
	function match(e:TestEnum, expected:Int) {
		var matched:Int = -1;
		
		matched = switch (e) {
			case Val1: 0;
			case null: 1;
			case _: 2;
		}
		
		eq(matched, expected);
	}
}

enum TestEnum {
	Val1;
	Val2;
}