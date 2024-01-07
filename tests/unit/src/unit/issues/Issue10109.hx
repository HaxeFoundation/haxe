package unit.issues;

class Issue10109 extends Test {
// Disabled for hl/c until https://github.com/HaxeFoundation/haxe/pull/11382 is implemented
#if (hl && !hlc)
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
#end
}

private enum Stop {
	A;
}
