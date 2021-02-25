package unit.issues;

class Issue10109 extends Test {
	@:pure(false)
	static function foo( o : String ) {
		return o.length;
	}

	function test() {
		try {
			try {
				foo(null);
			} catch( e : Stop ) {
			}
			assert();
		} catch(e) {
			t(e.stack.length > 0);
		}
	}
}

private enum Stop {
	A;
}
