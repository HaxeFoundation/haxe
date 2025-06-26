package unit.issues;

class Issue9957 extends unit.Test {
	function test() {
		var stat = staticExc;
		var inst = instanceExc;

		try {
			try stat()
			catch(_:MyEx) noAssert();
		} catch(_) {
			assert("Can't catch exceptions from static closures");
		}

		try {
			try inst()
			catch(_:MyEx) noAssert();
		} catch(_) {
			assert("Can't catch exceptions from instance closures");
		}
	}

	static function staticExc() {
		throw new MyEx('');
	}

	function instanceExc() {
		throw new MyEx('');
	}
}

private class MyEx extends haxe.Exception {}