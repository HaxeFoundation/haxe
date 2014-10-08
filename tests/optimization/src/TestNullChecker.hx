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

	function getString() {
		return "foo";
	}

	function getNullString():Null<String> {
		return null;
	}
}