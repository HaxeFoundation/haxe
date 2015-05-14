package ;

@:analyzer(no_check_has_effect)
class TestNullChecker extends TestBase {

	static function main() {
		new TestNullChecker();
	}

	public function new() {
		super();
		TestBaseMacro.run();
	}

	function test1() {
		var ns = getNullString();
		@:analyzer(testIsNull) ns;
		ns = "foo";
		@:analyzer(testIsNotNull) ns;
	}

	function test2() {
		var s = getString();
		@:analyzer(testIsNotNull) s;
		s = getNullString();
		@:analyzer(testIsNull) s;
	}

	function test3() {
		var ns = getNullString();
		if (ns == null) {
			@:analyzer(testIsNull) ns;
			ns = getString();
			@:analyzer(testIsNotNull) ns;
		}
		@:analyzer(testIsNotNull) ns;
	}

	function test4() {
		var ns = getNullString();
		if (ns != null) {
			@:analyzer(testIsNotNull) ns;
			ns = getNullString();
			@:analyzer(testIsNull) ns;
		}
		@:analyzer(testIsNull) ns;
	}

	function test5() {
		var ns = getNullString();
		if (ns != null) {
			@:analyzer(testIsNotNull) ns;
		} else {
			@:analyzer(testIsNull) ns;
			ns = getString();
		}
		@:analyzer(testIsNotNull) ns;
	}

	function test6() {
		var ns = getNullString();
		if (ns != null) {
			@:analyzer(testIsNotNull) ns;
		} else {
			if (ns == null) {
				ns = getString();
			}
		}
		@:analyzer(testIsNotNull) ns;
	}

	function testReturn1() {
		var ns = getNullString();
		if (ns == null) {
			return;
		}
		@:analyzer(testIsNotNull) ns;
	}

	function testReturn2() {
		var ns = getNullString();
		if (ns != null) {

		} else {
			return;
		}
		@:analyzer(testIsNotNull) ns;
	}

	// doesn't work yet due to || transformation
	//function testReturn3() {
		//var ns = getNullString();
		//if (ns == null || getTrue()) {
			//return;
		//}
		//@:analyzer(testIsNotNull) ns;
	//}

	function testReturn4() {
		var ns = getNullString();
		if (ns != null && getTrue()) {

		} else {
			return;
		}
		@:analyzer(testIsNull) ns;
	}

	function testBreak() {
		var ns = getNullString();
		while (true) {
			if (ns == null) {
				break;
			}
			@:analyzer(testIsNotNull) ns;
		}
		@:analyzer(testIsNull) ns;
	}

	function testContinue() {
		var ns = getNullString();
		while (true) {
			if (getTrue()) {
				break; // to terminate
			}
			if (ns == null) {
				continue;
			}
			@:analyzer(testIsNotNull) ns;
		}
		@:analyzer(testIsNull) ns;
	}

	function testThrow() {
		var ns = getNotNullString();
		if (ns == null) {
			throw false;
		}
		@:analyzer(testIsNotNull) ns;
	}

	function getString() {
		return "foo";
	}

	function getNullString():Null<String> {
		return null;
	}

	function getNotNullString():Null<String> {
		return "foo";
	}

	function getTrue() {
		return true;
	}

	function getFalse() {
		return false;
	}
}