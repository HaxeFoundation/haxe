package unit.issues;

class Issue4481 extends unit.Test {
	static var s:String = null;

	function test() {
		switch s {
			case 'a': assert();
			case _: eq(null, s);
		}

		eq(1, switch s {
			case 'a': 0;
			case _: 1;
		});

		switch s {
			case '': assert();
		}
	}
}