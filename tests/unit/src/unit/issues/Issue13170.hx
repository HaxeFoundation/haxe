package unit.issues;

class Issue13170 extends unit.Test {
	// TODO: enable on all targets once they support `finally`
	#if (js || eval)
	var log:Array<String>;

	function setup() {
		log = [];
	}

	function maybeThrow(b:Bool) {
		if (b)
			throw "error";
	}

	function returnInTry():Int {
		var x = 1;
		try {
			log.push("try");
			return x;
		} finally {
			x = 2;
			log.push("finally");
		}
	}

	function testReturn() {
		eq(1, returnInTry());
		eq("try,finally", log.join(","));
	}

	function testNormal() {
		try {
			log.push("try");
		} catch (e:String) {
			log.push("catch");
		} finally {
			log.push("finally");
		}
		eq("try,finally", log.join(","));
	}

	function testCaught() {
		var x = 0;
		try {
			x = 1;
			maybeThrow(true);
			x = 2;
		} catch (e:String) {
			log.push('catch $x');
		} finally {
			log.push('finally $x');
		}
		eq("catch 1,finally 1", log.join(","));
	}

	function testUncaught() {
		try {
			try {
				maybeThrow(true);
			} catch (e:Int) {
				log.push("wrong catch");
			} finally {
				log.push("finally");
			}
		} catch (e:String) {
			log.push('outer $e');
		}
		eq("finally,outer error", log.join(","));
	}

	function testThrowInCatch() {
		try {
			try {
				maybeThrow(true);
			} catch (e:String) {
				log.push("catch");
				throw "other";
			} finally {
				log.push("finally");
			}
		} catch (e:String) {
			log.push('outer $e');
		}
		eq("catch,finally,outer other", log.join(","));
	}

	function testThrowInFinally() {
		try {
			try {
				maybeThrow(true);
			} finally {
				throw "replaced";
			}
		} catch (e:String) {
			log.push(e);
		}
		eq("replaced", log.join(","));
	}

	function testLoop() {
		for (i in 0...4) {
			try {
				if (i == 1)
					continue;
				if (i == 3)
					break;
				log.push('try $i');
			} finally {
				log.push('finally $i');
			}
		}
		eq("try 0,finally 0,finally 1,try 2,finally 2,finally 3", log.join(","));
	}

	function testBreakValue() {
		var x = 0;
		while (true) {
			try {
				x = 1;
				break;
			} finally {
				x += 10;
			}
		}
		eq(11, x);
	}

	function nestedReturn():Int {
		try {
			try {
				return 1;
			} finally {
				log.push("inner");
			}
		} finally {
			log.push("outer");
		}
	}

	function testNested() {
		eq(1, nestedReturn());
		eq("inner,outer", log.join(","));
	}

	function testValue() {
		var x = try {
			log.push("try");
			1;
		} catch (e:String) {
			2;
		} finally {
			log.push("finally");
		}
		eq(1, x);
		eq("try,finally", log.join(","));
	}

	function testInitInFinally() {
		var x:Int;
		try {
			log.push("try");
		} finally {
			x = 1;
		}
		eq(1, x);
	}

	function testClosure() {
		var x = 0;
		var f = () -> x;
		try {
			x = 1;
		} finally {
			x++;
		}
		eq(2, f());
	}
	#end
}
